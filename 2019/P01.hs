module P01 where

main :: String -> IO ()
main txt =
  do let input = map read (lines txt)
     putStrLn "Problem 01"
     print (solve input fuel)
     print (solve input fuel2)

fuel :: Integer -> Integer
fuel m = div m 3 - 2

fuel2 :: Integer -> Integer
fuel2 m
  | f < 0     = 0
  | otherwise = f + fuel2 f
  where f = fuel m

solve :: [Integer] -> (Integer -> Integer) -> Integer
solve inp how = sum (map how inp)

