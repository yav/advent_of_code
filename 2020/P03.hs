module P03 where

import Data.List(unfoldr)

main :: String -> IO ()
main input =
  do print (doSlope terrain (3,1))
     print $ product $ map (doSlope terrain) [(1,1),(3,1),(5,1),(7,1),(1,2)]
  where
  terrain = parseInput input

type Terrain = [String]

parseInput :: String -> Terrain
parseInput = map cycle . lines

step :: Int -> Int -> Terrain -> Maybe (Int, Terrain)
step right down terrain =
  case map (drop right) (drop down terrain) of
    it@((c:_) : _) -> Just (if c == '#' then 1 else 0, it)
    _              -> Nothing

doSlope :: Terrain -> (Int,Int) -> Int
doSlope t (right,down) = sum (unfoldr (step right down) t)

example :: String
example = unlines
  [ "..##.........##.........##.........##.........##.........##......."
  , "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#.."
  , ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#."
  , "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#"
  , ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#."
  , "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##....."
  , ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#"
  , ".#........#.#........#.#........#.#........#.#........#.#........#"
  , "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#..."
  , "#...##....##...##....##...##....##...##....##...##....##...##....#"
  , ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
  ]
