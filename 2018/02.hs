import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe(catMaybes)
import Control.Monad(mplus,msum)

main :: IO ()
main =
  do txt <- parseInput "inputs/2.txt"
     let a = part1 txt
         b = part2 txt
     print (a,b)


part1 :: [String] -> Integer
part1 = uncurry (*) . foldr count (0,0)
  where
  countId i = Set.fromList
            $ Map.elems
            $ Map.fromListWith (+)
            $ zip i
            $ repeat 1

  has i s = if i `Set.member` s then 1 else 0

  count i (two,three) = let s = countId i
                        in (two + has 2 s, three + has 3 s)

part2 :: [String] -> Maybe String
part2 xs =
  case xs of
    x : xs' -> msum (map (diff1 x) xs') `mplus` part2 xs'
    []      -> Nothing

  where
  diff1 xs ys = if count == 1 then Just sames else Nothing
    where
    count = sum cmps
    cmps  = zipWith (\x y -> if x == y then 0 else 1) xs ys
    sames = catMaybes
          $ zipWith (\b x -> if b == 0 then Just x else Nothing) cmps xs


parseInput :: FilePath -> IO [String]
parseInput file = lines <$> readFile file

