{-# Language RecursiveDo #-}
module P07 where

import Data.List
import Control.Concurrent
import Control.Monad
import Data.IORef

import VM

main :: String -> IO ()
main txt =
  do mem <- parseProgram txt
     print =<< searchPhase 1 mem
     print =<< searchPhase 2 mem

ex1, ex2, ex3 :: String
ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
ex3 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

searchPhase :: Int -> Mem -> IO Value
searchPhase part mem =
  do let p : rest = permutations rng
     start <- doPhase p
     foldM check start rest
  where
  (rng,isRec) = if part == 1 then ([0..4],False) else ([5..9],True)
  doPhase     = startPhase isRec mem

  check big x =
    do next <- doPhase x
       pure $! max big next


startPhase :: Bool -> Mem -> [Value] -> IO Value
startPhase isRec mem vs =
  do let init1 : rest = reverse (zip [ 0 .. ] vs)

     rec outChan <- if isRec then pure firstIn else newChan
         ~lastVM <- mkVM outChan init1
         ~(_,firstIn) <- foldM (mkVM . snd) lastVM rest

     writeChan firstIn 0
     takeMVar (vmDone (fst lastVM))
     readChan outChan

  where
  mkVM o (n,v) =
    do i <- newChan
       writeChan i v
       m <- cloneMem mem
       d <- newEmptyMVar
       b <- newIORef 0
       let g = readChan i
           p = writeChan o
       let vm = VM { vmName = n, vmMem = m, vmGet = g, vmPut = p, vmDone = d
                   , vmBase = b }
       _ <- forkIO (runProgram vm)
       pure (vm,i)





