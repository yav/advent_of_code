{-# Language BlockArguments #-}
module P07 where

import Data.List
import Control.Concurrent
import Control.Monad

import VM

main :: String -> IO ()
main txt =
  do mem <- parseProgram txt
     print =<< searchPhase mem

ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

searchPhase :: Mem -> IO Value
searchPhase mem =
  do let p : rest = phases
     let search big todo =
           case todo of
             [] -> pure big
             x : more -> do next <- startPhase mem x
                            let bigger = max big next
                            bigger `seq` search bigger more
     start <- startPhase mem p
     search start rest


phases :: [[Value]]
phases = do opts <- replicateM 5 [ 0 .. 4 ]
            guard (length (nub opts) == 5)
            pure opts

startPhase :: Mem -> [Value] -> IO Value
startPhase mem vs =
  do putStr ("Phase: " ++ show vs)
     vms <- newChain mem vs [ [c] | c <- [ 'A' .. ] ]
     writeChan (vmIn (head vms)) 0
     a <- readChan (vmOut (last vms))
     print a
     pure a

newChain :: Mem -> [Value] -> [String] -> IO [VM]
newChain mem vs names =
  case vs of
    []  -> pure []
    [v] -> do vm0 <- newVM mem
              let vm = vm0 { vmName = head names }
              _ <- forkIO (runProgram vm)
              writeChan (vmIn vm) v
              pure [vm]
    v : more ->
      do vms <- newChain mem more (tail names)
         vm0 <- newVM mem
         let vm = vm0 { vmOut = vmIn (head vms), vmName = head names }
         _ <- forkIO (runProgram vm)
         writeChan (vmIn vm) v
         pure (vm : vms)





