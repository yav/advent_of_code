module Main(main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(foldl')
import Data.Maybe(mapMaybe)
import Text.Read(readMaybe)

main :: IO ()
main =
  do inp <- parseInput "inputs/9.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

part1 :: (Int,Int) -> Maybe Int
part1 (x,y) = case Map.elems s of
                [] -> Nothing
                vs -> Just (maximum vs)
  where (_,s) = play x y

part2 :: (Int,Int) -> Maybe Int
part2 (x,y) = part1 (x,y*100)


data Circle = Circle { counter :: [Int], current :: Int, clock :: [Int] }

single :: Int -> Circle
single x = Circle { counter = [], current = x, clock = [] }

goClock :: Circle -> Circle
goClock c =
  case clock c of
    [] -> case reverse (counter c) of
            [] -> c
            x : xs -> Circle { counter = [current c], current = x, clock = xs }
    x : xs -> Circle { counter = current c : counter c
                     , current = x
                     , clock   = xs
                     }

goCounter :: Circle -> Circle
goCounter c =
  case counter c of
    [] -> case reverse (clock c) of
            [] -> c
            x : xs -> Circle { counter = xs, current = x, clock = [current c] }
    x : xs -> Circle { counter = xs, current = x, clock = current c : clock c }


rpt :: Int -> (Circle -> Circle) -> Circle -> Circle
rpt n f c = iterate f c !! n

ins :: Int -> Circle -> Circle
ins x c = c { current = x, counter = current c : counter c }

del :: Circle -> Circle
del c = case clock c of
          x : xs -> c { current = x, clock = xs }
          [] -> case reverse (counter c) of
                  [] -> error "Can't delete last element"
                  x : xs -> c { current = x, clock = xs, counter = [] }

gameAdd :: Int -> Circle -> (Int,Circle)
gameAdd x c = case x `mod` 23 of
                0 -> let c1 = rpt 7 goCounter c
                     in (x + current c1, del c1)
                _ -> (0, ins x (goClock c))

playerTurns :: Int -> [Int]
playerTurns n = cycle [1..n]

marbles :: Int -> [Int]
marbles n = take n [ 1 .. ]

takeTurn :: (Circle, Map Int Int) -> (Int,Int) -> (Circle,Map Int Int)
takeTurn (c,scores) (p,m) = (c1, Map.insertWith (+) p score scores)
  where (score,c1) = gameAdd m c

play :: Int -> Int -> (Circle,Map Int Int)
play pNum mNum = foldl' takeTurn start (zip (playerTurns pNum) (marbles mNum))
  where start = (single 0, Map.empty)




parseInput :: FilePath -> IO (Int,Int)
parseInput file =
  do txt <- readFile file
     case mapMaybe readMaybe (words txt) of
       [a,b] -> pure (a,b)
       _ -> fail "Failed to parse input"

