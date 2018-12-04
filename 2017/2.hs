module Main where

import Data.Maybe(fromMaybe)
import Control.Monad(msum)

main :: IO ()
main =
  do sheet <- parseFile <$> readFile "input/2.txt"
     print (solution1 sheet)
     print (solution2 sheet)

parseFile :: String -> [[Int]]
parseFile = map parseRow . lines
  where parseRow = map read . words

solution1 :: [[Int]] -> Int
solution1 = sum . map checkRow

solution2 :: [[Int]] -> Int
solution2 = sum . map findDiv

checkRow :: [Int] -> Int
checkRow xs = maximum xs - minimum xs

findDiv :: [Int] -> Int
findDiv rs = fromMaybe 1 (msum [ divides x y | x <- rs, y <- rs ])
  where
  divides x y
    | y == 0    = Nothing
    | x == y    = Nothing
    | otherwise = case divMod x y of
                    (q,0) -> Just q
                    _     -> Nothing

