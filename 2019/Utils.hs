module Utils where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep xs =
  case break (== sep) xs of
    (as,_:bs) -> as : splitOn sep bs
    (as,[])   -> [as]


