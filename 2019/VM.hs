module VM where

import Text.Read(readMaybe)
import Data.List(unfoldr)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector
import Control.Monad(zipWithM_)
import Control.Concurrent

data VM = VM { vmMem  :: Mem
             , vmIn   :: Chan Value
             , vmOut  :: Chan Value
             , vmName :: Int
             , vmDone :: MVar ()  -- gets filled in when finished
             }

type Mem      = IOVector Value
newtype Addr  = Addr Int
type Value    = Int
data Mode     = Position | Immediate

(.+) :: Addr -> Int -> Addr
Addr x .+ y = Addr (x + y)

readMem :: VM -> Addr -> IO Value
readMem vm (Addr a) = Vector.read (vmMem vm) a

writeMem :: VM -> Addr -> Value -> IO ()
writeMem vm (Addr a) v = Vector.write (vmMem vm) a v

ptr :: VM -> Addr -> IO Addr
ptr vm a = Addr <$> readMem vm a

readIndirect :: VM -> Addr -> IO Value
readIndirect vm a = readMem vm =<< ptr vm a

writeIndirect :: VM -> Addr -> Value -> IO ()
writeIndirect vm a v =
  do addr <- ptr vm a
     writeMem vm addr v

dumpMem :: VM -> IO ()
dumpMem vm = print =<< Vector.freeze (vmMem vm)

readIn :: VM -> IO Value
readIn vm = readChan (vmIn vm)

writeOut :: VM -> Value -> IO ()
writeOut vm v = writeChan (vmOut vm) v

readArg :: VM -> Addr -> Mode -> IO Value
readArg vm addr mode =
  case mode of
    Immediate -> readMem vm addr
    Position  -> readIndirect vm addr

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
     mem <- Vector.new (length vals)
     zipWithM_ (Vector.write mem) [ 0 .. ] vals
     pure mem

  where
  next xs = case break (== ',') xs of
              (as,bs) -> do a <- readMaybe as
                            pure (a, drop 1 bs)

--------------------------------------------------------------------------------

newVM :: Mem -> IO VM
newVM mem =
  do i <- newChan
     o <- newChan
     m <- Vector.clone mem
     d <- newEmptyMVar
     pure VM { vmIn = i, vmOut = o, vmMem = m, vmDone = d, vmName = 0 }

runProgramFrom :: VM -> Addr -> IO ()
runProgramFrom vm pc =
  do mb <- doInstruction vm pc
     case mb of
       Nothing  -> pure ()
       Just pc1 -> runProgramFrom vm pc1



runProgram :: VM -> IO ()
runProgram vm = runProgramFrom vm (Addr 0)

test :: String -> IO ()
test inp =
  do vm <- newVM =<< parseProgram inp
     dumpMem vm
     runProgram vm
     dumpMem vm



--------------------------------------------------------------------------------



doInstruction :: VM -> Addr -> IO (Maybe Addr)
doInstruction vm pc =
  do opcode <- readMem vm pc
     let getMode arg =
           case mod (div opcode (10 ^ (2 + arg))) 10 of
             0 -> Position
             1 -> Immediate
             it -> error ("Unknown mode for operand " ++ show arg ++
                                                 ": " ++ show it)

         instr    = mod opcode 100
         getArg i = readArg vm (pc .+ (i+1)) (getMode i)

         bin op = do x <- getArg 0
                     y <- getArg 1
                     writeIndirect vm (pc .+ 3) (op x y)
                     pure (Just (pc .+ 4))

     case instr of
       99 -> do putMVar (vmDone vm) ()
                pure Nothing
       01 -> bin (+)
       02 -> bin (*)
       03 -> do debug vm "Getting"
                i <- readIn vm
                debug vm ("Got: "  ++ show i)
                writeIndirect vm (pc .+ 1) i
                pure (Just (pc .+ 2))
       04 -> do debug vm "Sending"
                writeOut vm =<< getArg 0
                pure (Just (pc .+ 2))
       05 -> do v   <- getArg 0
                tgt <- getArg 1
                pure (Just $ if v == 0 then pc .+ 3 else Addr tgt)
       06 -> do v   <- getArg 0
                tgt <- getArg 1
                pure (Just $ if v == 0 then Addr tgt else pc .+ 3)
       07 -> bin (\x y -> if x <  y then 1 else 0)
       08 -> bin (\x y -> if x == y then 1 else 0)


       _  -> fail ("Invalid opcode " ++ show opcode)





