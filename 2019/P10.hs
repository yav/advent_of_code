module P10 where

import Text.Show.Pretty

main :: String -> IO ()
main txt =
  do let pts = parseInput txt
     print $ maximum $ map snd $ check pts


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

parseInput :: String -> [Pt]
parseInput = concat . zipWith mkRow [ 0 .. ] . lines
  where
  mkRow y cs = [ (x,y) | (x,'#') <- zip [0..] cs ]

type Pt = (Int,Int)

between :: Pt -> Pt -> Pt -> Bool
between (me_x,me_y) (you_x,you_y) (tgt_x,tgt_y)
  | dx == 0   = tgt_x == me_x &&
                (if me_y < you_y then me_y <= tgt_y && tgt_y <= you_y
                                 else you_y <= tgt_y && tgt_y <= me_y)
  | otherwise = 0 <= k && k < 1 && dx * tgt_y == dx * me_y + dy * (tgt_x - me_x)
  where
  dx    = you_x - me_x
  dy    = you_y - me_y
  k     = fromIntegral (tgt_x - me_x) / fromIntegral dx

obstructedcFrom :: Pt -> Pt -> [Pt] -> Bool
obstructedcFrom me you = any (between me you)

pickOne :: [a] -> [(a,[a])]
pickOne xs =
  case xs of
    [] -> []
    a : more -> (a,more) : [ (b,a:bs) | (b,bs) <- pickOne more ]

-- check :: [Pt] -> [(Pt,Int)]
check xs =
  do (me,rest) <- pickOne xs
     let see = [ (you, blockers)
               | (you,others) <- pickOne rest
               , let blockers = [ b | b <- others, between me you b ]
               , null blockers
               ]
     pure (me,length see)


