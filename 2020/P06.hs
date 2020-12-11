module P06 where

import Data.List(nub,unfoldr)
import qualified Data.Set as Set

main :: String -> IO ()
main input =
  do doWork (length . nub . concat)
     doWork (Set.size . foldr1 Set.intersection . map Set.fromList)
  where
  groups = parseInput input
  doWork f = print $ sum $ map f groups

type Group = [String]

parseInput :: String -> [Group]
parseInput = unfoldr nextGroup . lines

nextGroup :: [String] -> Maybe (Group,[String])
nextGroup ls =
  case dropWhile null ls of
    [] -> Nothing
    _  -> case break null ls of
            (as,bs) -> Just (as, drop 1 bs)

