module P09 where

import Control.Concurrent

import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 09"
     let txt = ex1
     mem <- parseProgram txt
     vm  <- newVM mem
     runProgram vm
     drain vm

ex1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
ex2 = "1102,34915192,34915192,7,4,7,99,0"
ex3 = "104,1125899906842624,99"

