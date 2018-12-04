module Main where

import Data.Char(isDigit)

main :: IO ()
main =
  do txt <- filter isDigit <$> readFile "input/1.txt"
     print (solution 1 txt)
     print (solution (div (length txt) 2) txt)

solution :: Int -> String -> Int
solution n xs = sum (zipWith check xs (drop n xs ++ xs))
  where
  check x y = if x == y then read [x] else 0

