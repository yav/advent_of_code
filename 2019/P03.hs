module P03 where

import Utils
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: String -> IO ()
main txt = do let input   = parseInput txt
                  sim     = map doMoves input
                  crosses = Set.toList
                          $ foldr1 Set.intersection
                          $ map Map.keysSet sim

                  bestBy f       = minimum $ filter (/= 0) $ map f crosses
                  distance (a,b) = abs a + abs b
                  timing x       = sum (map (Map.! x) sim)

              print (bestBy distance)
              print (bestBy timing)

ex1, ex2 :: String
ex1 = unlines [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
              , "U62,R66,U55,R34,D71,R55,D58,R83"
              ]
ex2 = unlines [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
              , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
              ]

parseInput :: String -> [ [(Dir,Int)] ]
parseInput = map (map parseMove . splitOn ',') . lines

data Dir = L | R | U | D deriving (Show,Read)

parseMove :: String -> (Dir,Int)
parseMove ~(x:xs) = (read [x], read xs)

move :: Dir -> (Int,Int) -> (Int,Int)
move dir (x,y) =
  case dir of
    L -> (x - 1, y)
    R -> (x + 1, y)
    U -> (x, y - 1)
    D -> (x, y + 1)

data S = S
  { curLoc  :: (Int,Int)
  , steps   :: Int
  , visited :: Map (Int,Int) Int -- location |-> first time we got there
  }

start :: S
start = S { visited = Map.singleton l 0, steps = 0, curLoc = l }
  where l = (0,0)

doMove :: (Dir,Int) -> S -> S
doMove (d,n) = (!! n) . iterate step
  where
  step s = let newLoc = move d (curLoc s)
               newSteps = steps s + 1
           in S { curLoc = newLoc
                , steps  = newSteps
                , visited = Map.insertWith min newLoc newSteps (visited s)
                }

doMoves :: [(Dir,Int)] -> Map (Int,Int) Int
doMoves = visited . foldl (flip doMove) start





