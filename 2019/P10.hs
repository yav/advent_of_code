module P10 where

import Data.List
import Data.Function

main :: String -> IO ()
main txt =
  do let pts      = parseInput txt
         (loc,n)  = maximumBy (compare `on` snd) (check pts)
         ps       = order (map (polar loc) (delete loc pts))
     print (loc,n)
     let (x,y) = pt (ps !! 199)
     print (100 * x + y)


ex1, ex2, ex3 :: String
ex1 = unlines
  [ ".#..#"
  , "....."
  , "#####"
  , "....#"
  , "...##"
  ]

ex2 = unlines
  [ "......#.#."
  , "#..#.#...."
  , "..#######."
  , ".#.#.###.."
  , ".#..#....."
  , "..#....#.#"
  , "#..#....#."
  , ".##.#..###"
  , "##...#..#."
  , ".#....####"
  ]

ex3 = unlines
  [ ".#..##.###...#######"
  , "##.############..##."
  , ".#.######.########.#"
  , ".###.#######.####.#."
  , "#####.##.#.##.###.##"
  , "..#####..#.#########"
  , "####################"
  , "#.####....###.#.#.##"
  , "##.#################"
  , "#####.##.###..####.."
  , "..######..##.#######"
  , "####.##.####...##..#"
  , ".#####..#.######.###"
  , "##...#.##########..."
  , "#.##########.#######"
  , ".####.#.###.###.#.##"
  , "....##.##.###..#####"
  , ".#.#.###########.###"
  , "#.#.#.#####.####.###"
  , "###.##.####.##.#..##"
  ]

ex4 :: String
ex4 = unlines
  [ ".#....#####...#.."
  , "##...##.#####..##"
  , "##...#...#.#####."
  , "..#.....X...###.."
  , "..#.#.....#....##"
  ]

parseInput :: String -> [Pt]
parseInput = concat . zipWith mkRow [ 0 .. ] . lines
  where
  mkRow y cs = [ (x,y) | (x,'#') <- zip [0..] cs ]

type Pt = (Int,Int)


between1 :: Int -> Int -> Int -> Bool
between1 me you other = smaller <= other && other <= bigger
  where smaller = min me you
        bigger  = max me you


between :: Pt -> Pt -> Pt -> Bool
between (me_x,me_y) (you_x,you_y) (tgt_x,tgt_y)
  | dx == 0   = tgt_x == me_x && between1 me_y you_y tgt_y
  | otherwise = 0 <= k && k < 1 && dx * tgt_y == dx * me_y + dy * (tgt_x - me_x)
  where
  dx    = you_x - me_x
  dy    = you_y - me_y
  k     = fromIntegral (tgt_x - me_x) / fromIntegral dx :: Double

obstructedcFrom :: Pt -> Pt -> [Pt] -> Bool
obstructedcFrom me you = any (between me you)

pickOne :: [a] -> [(a,[a])]
pickOne xs =
  case xs of
    [] -> []
    a : more -> (a,more) : [ (b,a:bs) | (b,bs) <- pickOne more ]

check :: [Pt] -> [(Pt,Int)]
check xs =
  do (me,rest) <- pickOne xs
     let see = [ (you, blockers)
               | (you,others) <- pickOne rest
               , let blockers = [ b | b <- others, between me you b ]
               , null blockers
               ]
     pure (me,length see)

-- | modified polar: angle is 0 at the +ve y-axis and goes clockwise
data Polar = Polar { pt :: (Int,Int), angle :: Double, distance :: Double }
  deriving Show

polar :: Pt -> Pt -> Polar
polar (x,y) (a,b) = Polar { pt       = (a,b)
                          , distance = sqrt (dx * dx + dy * dy)
                          , angle    = let r = atan2 dx (-dy)
                                       in if r < 0 then 2*pi + r else r
                          }
  where dx = fromIntegral (a - x)
        dy = fromIntegral (b - y) :: Double


order :: [Polar] -> [Polar]
order  = concat . transpose
       .map (sortBy (compare `on` distance))
      . groupBy ((==) `on` angle)
      . sortBy (compare `on` angle)



