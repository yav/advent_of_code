module P05 where

import Control.Concurrent
import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 5"
     prog <- parseProgram txt

     vm1 <- newVM prog
     _ <- forkIO (runProgram vm1)
     writeChan (vmIn vm1) 1
     drain vm1

     vm2 <- newVM prog
     _ <- forkIO (runProgram vm2)
     writeChan (vmIn vm2) 5
     drain vm2


