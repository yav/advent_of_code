module P01 where

main :: String -> IO ()
main txt = do problem 2
              problem 3
  where
  input     = parseInput txt
  problem n = print $ product $ head $ search n input

parseInput :: String -> [Integer]
parseInput = map read . lines

pairs :: Int -> [Integer] -> [[Integer]]
pairs n xs
  | n < 1 = [ [] ]
  | otherwise =
    case xs of
      x : ys -> [ x : p | p <- pairs (n-1) ys ] ++ pairs n ys
      []     -> []

suitable :: [Integer] -> Bool
suitable = (== 2020) . sum

search :: Int -> [Integer] -> [[Integer]]
search n = filter suitable . pairs n

