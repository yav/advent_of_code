module P02 where

import Data.List(unfoldr)
import Text.Read(readMaybe)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector
import Control.Monad(zipWithM_)

main :: String -> IO ()
main txt =
  do putStrLn "Problem 02"
     mem <- parseInput txt
     print =<< runWithInput mem 12 2
     part2 mem [ (noun,verb) | noun <- [ 0 .. 99 ], verb <- [ 0 .. 99 ] ]


part2 :: IOVector Int -> [(Int,Int)] -> IO ()
part2 mem opts =
  case opts of
    (noun,verb) : more ->
      do ans <- runWithInput mem noun verb
         if ans == 19690720 then print (100 * noun + verb) else part2 mem more
    [] -> print "No solution found"


runWithInput :: IOVector Int -> Int -> Int -> IO Int
runWithInput template x y =
  do mem <- Vector.clone template
     Vector.write mem 1 x
     Vector.write mem 2 y
     runProgram mem 0
     Vector.read mem 0


test :: String -> IO ()
test inp =
  do mem <- parseInput inp
     print =<< Vector.freeze mem
     runProgram mem 0
     print =<< Vector.freeze mem

runProgram :: IOVector Int -> Int -> IO ()
runProgram mem pc =
  do mb <- doInstruction mem pc
     case mb of
       Nothing  -> pure ()
       Just pc1 -> runProgram mem pc1

doInstruction :: IOVector Int -> Int -> IO (Maybe Int)
doInstruction mem pc =
  do opcode <- Vector.read mem pc
     case opcode of
       99 -> pure Nothing
       01 -> bin (+)
       02 -> bin (*)
       _  -> fail ("Invalid opcode " ++ show opcode)
  where
  getVal n   = Vector.read  mem n   :: IO Int
  setVal x n = Vector.write mem n x :: IO ()

  bin op = do x <- getVal =<< getVal (pc + 1)
              y <- getVal =<< getVal (pc + 2)
              setVal (op x y) =<< getVal (pc + 3)
              pure (Just (pc + 4))

parseInput :: String -> IO (IOVector Int)
parseInput inp =
  do let vals = unfoldr next inp
     mem <- Vector.new (length vals)
     zipWithM_ (Vector.write mem) [ 0 .. ] vals
     pure mem

  where
  next xs = case break (== ',') xs of
              (as,bs) -> do a <- readMaybe as
                            pure (a, drop 1 bs)

