{-# Language BlockArguments #-}
module VM where

import Text.Read(readMaybe)
import Data.List(unfoldr)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad(zipWithM_,forM_)
import Control.Concurrent

data VM = VM { vmMem  :: Mem
             , vmBase :: IORef Value
             , vmGet  :: IO Value
             , vmPut  :: Value -> IO ()
             , vmName :: Int
             , vmDone :: MVar ()  -- gets filled in when finished
             }



pageSize :: Int
pageSize = 4096

type Mem      = IORef (Map Int (IOVector Value))
newtype Addr  = Addr Int
type Value    = Integer
data Mode     = Position | Relative | Immediate

addr :: Value -> Addr
addr x = Addr 0 .+ x

(.+) :: Addr -> Value -> Addr
Addr x .+ y = if new < 0 || new > m then error "Onvalid addres"
                                    else Addr (fromInteger new)
  where new = toInteger x + y
        m   = toInteger (maxBound :: Int)

newMem :: IO Mem
newMem = newIORef Map.empty

cloneMem :: Mem -> IO Mem
cloneMem ref = newIORef =<< traverse Vector.clone =<< readIORef ref

readMem' :: Mem -> Addr -> IO Value
readMem' ref (Addr a) =
  do mem <- readIORef ref
     let (pageIx,off) = divMod a pageSize
     case Map.lookup pageIx mem of
       Nothing   -> pure 0
       Just page -> Vector.read page off

writeMem' :: Mem -> Addr -> Value -> IO ()
writeMem' ref (Addr a) v =
  do mem <- readIORef ref
     let (pageIx,off) = divMod a pageSize
     case Map.lookup pageIx mem of
       Just page -> Vector.write page off v
       Nothing -> do page <- Vector.new pageSize
                     Vector.set page 0
                     Vector.write page off v
                     writeIORef ref $! Map.insert pageIx page mem

dumpMem' :: Mem -> IO ()
dumpMem' ref =
  do mem <- readIORef ref
     forM_ (Map.toList mem) \(i,p) ->
        do putStrLn ("page " ++ show i)
           print =<< Vector.freeze p

--------------------------------------------------------------------------------

readMem :: VM -> Addr -> IO Value
readMem = readMem' . vmMem

writeMem :: VM -> Addr -> Value -> IO ()
writeMem = writeMem' . vmMem

dumpMem :: VM -> IO ()
dumpMem = dumpMem' . vmMem

ptr :: VM -> Addr -> IO Addr
ptr vm a = addr <$> readMem vm a

readIndirect :: VM -> Addr -> IO Value
readIndirect vm a = readMem vm =<< ptr vm a

writeIndirect :: VM -> Addr -> Value -> IO ()
writeIndirect vm a v =
  do x <- ptr vm a
     writeMem vm x v

readArg :: VM -> Addr -> Mode -> IO Value
readArg vm a mode =
  case mode of
    Immediate -> readMem vm a
    Position  -> readIndirect vm a
    Relative  -> do b <- readIORef (vmBase vm)
                    v <- readMem vm a
                    readMem vm (addr (b+v))

writeArg :: VM -> Addr -> Mode -> Value -> IO ()
writeArg vm a mode x =
  case mode of
    Immediate -> error "Inavlid write mode"
    Position  -> writeIndirect vm a x
    Relative  -> do b <- readIORef (vmBase vm)
                    v <- readMem vm a
                    writeMem vm (addr (b+v)) x

debug :: VM -> String -> IO ()
debug vm x
  | dbg       = putStrLn ("[" ++ show (vmName vm) ++ "] " ++ x)
  | otherwise = pure ()
  where
  dbg = False


--------------------------------------------------------------------------------

parseProgram :: String -> IO Mem
parseProgram inp =
  do let vals = unfoldr next inp
     mem <- newMem
     zipWithM_ (writeMem' mem) (map Addr [ 0 .. ]) vals
     pure mem

  where
  next xs = case break (== ',') xs of
              (as,bs) -> do a <- readMaybe as
                            pure (a, drop 1 bs)

--------------------------------------------------------------------------------

newVM :: Mem -> IO Value -> (Value -> IO ()) -> IO VM
newVM mem g p =
  do m <- cloneMem mem
     d <- newEmptyMVar
     b <- newIORef 0
     pure VM { vmGet = g, vmPut = p, vmMem = m, vmDone = d, vmName = 0
             , vmBase = b }

runProgramFrom :: VM -> Addr -> IO ()
runProgramFrom vm pc =
  do mb <- doInstruction vm pc
     case mb of
       Nothing  -> pure ()
       Just pc1 -> runProgramFrom vm pc1



runProgram :: VM -> IO ()
runProgram vm = runProgramFrom vm (Addr 0)



--------------------------------------------------------------------------------



doInstruction :: VM -> Addr -> IO (Maybe Addr)
doInstruction vm pc =
  do opcode <- readMem vm pc
     let getMode arg =
           case mod (div opcode (10 ^ (2 + arg))) 10 of
             0 -> Position
             1 -> Immediate
             2 -> Relative
             it -> error ("Unknown mode for operand " ++ show arg ++
                                                 ": " ++ show it)

         instr    = mod opcode 100
         getArg i = readArg vm (pc .+ (i+1)) (getMode i)
         resultIn i = writeArg vm (pc .+ (i+1)) (getMode i)

         bin op = do x <- getArg 0
                     y <- getArg 1
                     resultIn 2 (op x y)
                     pure (Just (pc .+ 4))

     case instr of
       01 -> bin (+)
       02 -> bin (*)
       03 -> do debug vm "Getting"
                i <- vmGet vm
                debug vm ("Got: "  ++ show i)
                resultIn 0 i
                pure (Just (pc .+ 2))
       04 -> do debug vm "Sending"
                vmPut vm =<< getArg 0
                pure (Just (pc .+ 2))
       05 -> do v   <- getArg 0
                tgt <- getArg 1
                pure (Just $ if v == 0 then pc .+ 3 else addr tgt)
       06 -> do v   <- getArg 0
                tgt <- getArg 1
                pure (Just $ if v == 0 then addr tgt else pc .+ 3)
       07 -> bin (\x y -> if x <  y then 1 else 0)
       08 -> bin (\x y -> if x == y then 1 else 0)
       09 -> do v <- getArg 0
                modifyIORef' (vmBase vm) (+ v)
                pure (Just (pc .+ 2))

       99 -> do putMVar (vmDone vm) ()
                pure Nothing
       _  -> fail ("Invalid opcode " ++ show opcode)





