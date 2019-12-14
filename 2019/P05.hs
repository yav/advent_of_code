module P05 where

import Control.Concurrent
import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 5"
     prog <- parseProgram txt

     vm1 <- newVM prog (pure 1) print
     runProgram vm1

     vm2 <- newVM prog (pure 5) print
     runProgram vm2


