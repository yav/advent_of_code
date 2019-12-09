module P02 where

import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 02"
     mem <- parseProgram txt
     print =<< runWithInput mem 12 2
     part2 mem [ (noun,verb) | noun <- [ 0 .. 99 ], verb <- [ 0 .. 99 ] ]


part2 :: Mem -> [(Int,Int)] -> IO ()
part2 mem opts =
  case opts of
    (noun,verb) : more ->
      do ans <- runWithInput mem noun verb
         if ans == 19690720 then print (100 * noun + verb) else part2 mem more
    [] -> print "No solution found"



