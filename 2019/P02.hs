module P02 where

import VM

main :: String -> IO ()
main txt =
  do putStrLn "Problem 02"
     mem <- parseProgram txt
     print =<< runWithInput mem 12 2
     part2 mem [ (noun,verb) | noun <- [ 0 .. 99 ], verb <- [ 0 .. 99 ] ]


part2 :: Mem -> [(Value,Value)] -> IO ()
part2 mem opts =
  case opts of
    (noun,verb) : more ->
      do ans <- runWithInput mem noun verb
         if ans == 19690720 then print (100 * noun + verb) else part2 mem more
    [] -> print "No solution found"


runWithInput :: Mem -> Value -> Value -> IO Value
runWithInput template x y =
  do vm <- newVM template
     writeMem vm (addr 1) x
     writeMem vm (addr 2) y
     runProgram vm
     readMem vm (addr 0)


