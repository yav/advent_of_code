module Main where

import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do input <- readIO =<< readFile "input/3.txt"
          print (solution input)
          print (search input start path)

solution :: Int -> Int
solution n = distances !! (n-1)

distances = 0 : concatMap ring [ 1 .. ]

ring n = concat (replicate 4 side)
  where
  nums  = [ n .. 2 * n ]
  down  = tail (reverse nums)
  up    = tail nums
  side  = down ++ up




start         = Map.singleton (0,0) 1

search g mp (p:ps)
  | new > g = new
  | otherwise = search g (Map.insert p new mp) ps
  where new = sum [ val n mp | n <- neighbours p ]

neighbours (x,y) = [ (x+dx,y+dy) | dx <- opts, dy <- opts,
                                  not (dx == 0 && dy == 0 )  ]
  where opts = [-1,0,1]

val x mp = Map.findWithDefault 0 x mp

east  (x,y) = (x + 1, y)
south (x,y) = (x,y + 1)
west  (x,y) = (x-1,y)
north (x,y) = (x, y - 1)

ringPath n = replicate (n-1) north
          ++ replicate n west
          ++ replicate n south
          ++ replicate (n+1) east

path = drop 1 $ scanl (\l f -> f l) (0,0) (concatMap ringPath [ 0, 2 .. ])


