module Main(main) where

import Data.Char(isDigit)
import Data.Maybe(mapMaybe,catMaybes,isJust)
import Text.Read(readMaybe)
import Control.Monad(zipWithM)
import qualified Data.Map as Map


main :: IO ()
main =
  do inp <- parseInput "inputs/6.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

type Point = (Int,Int)

part1 :: [Point] -> Int
part1 inp = maximum $ catMaybes $ Map.elems $ foldr count Map.empty owners
  where
  box     = boundingBox inp
  pts     = pointsOf box
  owners  = mapMaybe (owner inp) pts   -- only the ones with concrete owner

  add sz1 sz2   = (+) <$> sz1 <*> sz2 -- Nothing: infinite
  count (pt,ow) = if onEdge box pt
                    then Map.insert ow Nothing
                    else Map.insertWith add ow (Just 1)


part2 :: [Point] -> Int
part2 inp = length totDists
  where
  ((x1,y1),(x2,y2))= boundingBox inp
  pnum     = length inp
  newBox   = ((x1 - pnum, y1 - pnum), (x2+pnum,y2+pnum))

  totDists = [ () | p <- pointsOf newBox
                  , let di = sum (map (distance p) inp)
                  , di < 10000 ]

distance :: Point -> Point -> Int
distance (a,b) (x,y) = abs (a - x) + abs (b - y)


-- | Assumes non-empty input
boundingBox :: [Point] -> (Point, Point)
boundingBox ps = ((x0,y0), (x1,y1))
  where
  (xs,ys)        = unzip ps
  minMax x (a,b) = (min x a, max x b)
  search         = foldr minMax (maxBound,minBound)
  (x0,x1)        = search xs
  (y0,y1)        = search ys

pointsOf :: (Point,Point) -> [Point]
pointsOf ((x0,y0),(x1,y1)) = [ (x,y) | x <- [ x0 .. x1 ], y <- [ y0 .. y1 ] ]

onEdge :: (Point,Point) -> Point -> Bool
onEdge ((x0,y0),(x1,y1)) (x,y) = x == x0 || x == x1 || y == y0 || y == y1

-- | Returns nothing if there are no points
owner :: [Point] -> Point -> Maybe (Point,Int)
owner ps p = case foldr smaller Unknown ds of
               Smallest n1 _  -> Just (p,n1)
               _              -> Nothing
  where
  ds               = zip [ 0 .. ] (map (distance p) ps)
  smaller (n,d) xs = case xs of
                       Unknown -> Smallest n d
                       Multiple d1 | d < d1 -> Smallest n d
                                   | otherwise -> Multiple d1
                       Smallest n1 d1 -> case compare d d1 of
                                           LT -> Smallest n d
                                           EQ -> Multiple d1
                                           _  -> Smallest n1 d1

data Status = Unknown | Multiple Int     -- distance
                      | Smallest Int Int -- owner, distance




--------------------------------------------------------------------------------
parseInput :: FilePath -> IO [(Int,Int)]
parseInput file =
  do txt <- readFile file
     zipWithM parseLine [ 1 .. ] (lines txt)
  where
  parseLine n l =
    case words (map norm l) of
      [a,b] | Just x <- readMaybe a, Just y <- readMaybe b -> pure (x,y)
      _ -> fail ("Parse error on line " ++ show (n::Int))

  norm c = if isDigit c then c else ' '
