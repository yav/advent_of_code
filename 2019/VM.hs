module VM where

import Text.Read(readMaybe)
import Data.List(unfoldr)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector
import Control.Monad(zipWithM_)

type Mem      = IOVector Value
newtype Addr  = Addr Int
type Value    = Int

(.+) :: Addr -> Int -> Addr
Addr x .+ y = Addr (x + y)

readMem :: Mem -> Addr -> IO Value
readMem mem (Addr a) = Vector.read mem a

writeMem :: Mem -> Addr -> Value -> IO ()
writeMem mem (Addr a) v = Vector.write mem a v

ptr :: Mem -> Addr -> IO Addr
ptr mem a = Addr <$> readMem mem a

readIndirect :: Mem -> Addr -> IO Value
readIndirect mem a = readMem mem =<< ptr mem a

writeIndirect :: Mem -> Addr -> Value -> IO ()
writeIndirect mem a v =
  do addr <- ptr mem a
     writeMem mem addr v


dumpMem :: Mem -> IO ()
dumpMem mem = print =<< Vector.freeze mem

cloneMem :: Mem -> IO Mem
cloneMem = Vector.clone

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


runProgram :: Mem -> Addr -> IO ()
runProgram mem pc =
  do mb <- doInstruction mem pc
     case mb of
       Nothing  -> pure ()
       Just pc1 -> runProgram mem pc1

test :: String -> IO ()
test inp =
  do mem <- parseProgram inp
     dumpMem mem
     runProgram mem (Addr 0)
     dumpMem mem



--------------------------------------------------------------------------------



doInstruction :: Mem -> Addr -> IO (Maybe Addr)
doInstruction mem pc =
  do opcode <- readMem mem pc
     case opcode of
       99 -> pure Nothing
       01 -> bin (+)
       02 -> bin (*)
       _  -> fail ("Invalid opcode " ++ show opcode)
  where
  bin op = do x <- readIndirect mem (pc .+ 1)
              y <- readIndirect mem (pc .+ 2)
              writeIndirect mem (pc .+ 3) (op x y)
              pure (Just (pc .+ 4))


