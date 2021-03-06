module P09 where

import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 09"
     mem <- parseProgram txt

     runProgram =<< newVM mem (pure 1) print
     runProgram =<< newVM mem (pure 2) print



ex1, ex2, ex3 :: String
ex1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
ex2 = "1102,34915192,34915192,7,4,7,99,0"
ex3 = "104,1125899906842624,99"

