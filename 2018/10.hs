module Main(main) where

import Control.Monad(zipWithM,replicateM_,foldM)
import Text.Read(readMaybe)
import Data.Maybe(mapMaybe)
import Data.Char(isDigit)
import Data.Function(on)
import Data.List(sortBy,groupBy)

main :: IO ()
main =
  do inp <- parseInput "inputs/10.txt"
     part1 inp

part1 :: [VPoint] -> IO ()
part1 inp =
  do let steps = iterate stepPoints inp
         sh s = do drawPts s
                   getLine
         ssteps = map bboxSize steps `zip` steps
         pairs = zip ssteps (tail ssteps)
         ok ((s1,_),(s2,_)) = s2 < s1
         bs = dropWhile ok pairs
         x = snd $ fst $ head bs
     drawPts x


part2 :: [(Point,Point)] -> String
part2 _ = ""

stepPoint :: VPoint -> VPoint
stepPoint ((x,y),(dx,dy)) = ((x+dx,y+dy),(dx,dy))

stepPoints :: [VPoint] -> [VPoint]
stepPoints = map stepPoint

bbox :: [VPoint] -> (Point,Point)
bbox vps = ((minimum xs,minimum ys),(maximum xs,maximum ys))
  where
  (xs,ys) = unzip (map fst vps)

bboxSize :: [VPoint] -> Int
bboxSize vps = (x2 - x1) * (y2 - y1)
  where
  ((x1,y1),(x2,y2)) = bbox vps

drawPts :: [VPoint] -> IO()
drawPts ps = do _ <- foldM drawRow (y1-1) rows
                return ()
  where
  ((x1,y1),_) = bbox ps

  drawRow l xs = do _ <- foldM drawPt (x1 - 1) (map fst xs)
                    let y = snd (head xs)
                    replicateM_ (y - l) (putStrLn "")
                    return y

  drawPt l x = do putStr (replicate (x - l - 1) ' ')
                  putChar '#'
                  return x

  rows = map (sortBy (compare `on` fst))
       $ groupBy ((==) `on` snd)
       $ sortBy (compare `on` snd)
       $ map fst ps




type Point = (Int,Int)
type VPoint = (Point,Point)

parseInput :: FilePath -> IO [VPoint]
parseInput file =
  do txt <- readFile file
     zipWithM parseLine [1::Int ..] (lines txt)
  where
  parseLine n l = case mapMaybe readMaybe (words (map cvt l)) of
                    [a,b,c,d] -> pure ((a,b),(c,d))
                    _         -> fail ("Parse error on line: " ++ show n)
  cvt x = if x == '-' || isDigit x then x else ' '

