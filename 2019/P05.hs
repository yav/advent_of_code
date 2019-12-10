module P05 where

import Control.Concurrent
import Control.Exception
import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 5"
     prog <- parseProgram txt

     vm1 <- newVM prog
     forkIO (runProgram vm1)
     writeChan (vmIn vm1) 1
     drain vm1

     vm2 <- newVM prog
     forkIO (runProgram vm2)
     writeChan (vmIn vm2) 5
     drain vm2

drain :: VM -> IO ()
drain vm = do mb <- try (readChan (vmOut vm))
              case mb of
                Left BlockedIndefinitelyOnMVar -> pure ()
                Right a -> print a >> drain vm

