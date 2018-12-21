import Data.Set(Set)
import qualified Data.Set as Set

main :: IO ()
main =
  do nums <- parseInput "inputs/1.txt"
     let a = part1 nums
         b = part2 nums
     print (a,b)

part1 :: [Integer] -> Integer
part1 = sum

part2 :: [Integer] -> Integer
part2 = search Set.empty . scanl (+) 0 . cycle
  where
  search seen xs =
    case xs of
      a : as -> if a `Set.member` seen
                      then a
                      else search (Set.insert a seen) as
      [] -> error "Impossible!"

parseInput :: FilePath -> IO [Integer]
parseInput file =
  do txt <- readFile file
     pure $ map read $ lines $ filter (/= '+') txt
