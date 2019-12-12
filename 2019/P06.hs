module P06 where

import Data.Map(Map)
import qualified Data.Map as Map

main :: String -> IO ()
main txt =
  do putStrLn "Problem 06"
     let input = parseInput txt
         info = build input
         part1 = sum (fst <$> info)
         part2 a b = let (_,xs) = info Map.! a
                         (_,ys) = info Map.! b
                         (p,q)  = dropCommon (reverse xs) (reverse ys)
                     in (length p + length q)
     print part1
     print (part2 "YOU" "SAN")


parseInput :: String -> [(String,String)]
parseInput = map split . lines
  where split x = case break (== ')') x of
                    (as,_:bs) -> (as,bs)
                    _ -> error "invalid input"

ex1 :: String
ex1 = unlines
  [ "COM)B"
  , "B)C"
  , "C)D"
  , "D)E"
  , "E)F"
  , "B)G"
  , "G)H"
  , "D)I"
  , "E)J"
  , "J)K"
  , "K)L"
  , "K)YOU"
  , "I)SAN"
  ]

-- type Info = Map String (Int,[String])
type Info = Map String (Int,[String])

build :: [(String,String)] -> Info
build xs = finMp
  where
  finMp = foldr (add finMp) Map.empty xs

add :: Info -> (String,String) -> Info -> Info
add finMp (x,y) mp = Map.insert y (n+1,x:xs) mp
  where
  (n,xs) = Map.findWithDefault (0,[]) x finMp

dropCommon :: Eq a => [a] -> [a] -> ([a],[a])
dropCommon xs ys =
  case (xs,ys) of
    (x:xs',y:ys') | x == y -> dropCommon xs' ys'
    _ -> (xs,ys)
