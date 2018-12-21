module Main(main) where

import Data.Char(isUpper,toUpper,isAlpha)

main :: IO ()
main =
  do inp <- filter isAlpha <$> readFile "inputs/5.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

part1 :: String -> Int
part1 = length . normalize

part2 :: String -> Int
part2 inp = minimum [ length (normalize (remove c inp)) | c <- [ 'a' .. 'z' ] ]

remove :: Char -> String -> String
remove x = filter (\y -> toUpper y /= x')
  where x' = toUpper x

normalize :: String -> String
normalize = foldr react []
  where
  react a xs = case xs of
                [] -> [a]
                b : bs -> if canReact a b then bs else a : xs

canReact :: Char -> Char -> Bool
canReact x y = (toUpper x == toUpper y) && (diff x y || diff y x)
    where diff a b = isUpper a && not (isUpper b)

check xs = or $ zipWith canReact xs (tail xs)
