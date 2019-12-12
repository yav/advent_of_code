{-# Language BlockArguments #-}
module P08 where

import Data.List
import Data.Ord

main :: String -> IO ()
main txt =
  do putStrLn "Problem 08"
     let input = parseInput 25 6 txt
     print (part1 input)

type Layer = [String]

parseInput :: Int -> Int -> String -> [Layer]
parseInput w h = splitInto h . splitInto w

splitInto :: Int -> [a] -> [[a]]
splitInto n = unfoldr \s -> case splitAt n s of
                              (as,bs) | length as == n -> Just (as,bs)
                              _ -> Nothing

data P3 = P3 !Int !Int !Int
  deriving Show

part1 :: [Layer] -> Int
part1 = mul . minimumBy (comparing zeros) . map countImg
  where
  zeros (P3 z _ _) = z
  mul (P3 _ d1 d2) = d1 * d2

  countImg      = foldr countRow (P3 0 0 0)
  countRow xs d = foldr addDigit d xs
  addDigit n s@(P3 d0 d1 d2) = case n of
                                 '0' -> P3 (d0+1) d1 d2
                                 '1' -> P3 d0 (d1+1) d2
                                 '2' -> P3 d0 d1 (d2+1)
                                 _   -> s

