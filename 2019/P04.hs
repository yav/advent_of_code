{-# Language BangPatterns #-}
module P04 where

import Data.List(group)

main :: String -> IO ()
main txt =
  do let (start,end) = case break (== '-') txt of
                         (a,_:b) -> (read a, read b)
                         _ -> error "invalid input"
         part1 = [ () | r <- [ start .. end ], fst (valid r) ]
         part2 = [ () | r <- [ start .. end ], snd (valid r) ]
     print (length part1)
     print (length part2)

examples = map valid [ 111111, 223450, 123789 ]

-- assumes positive
leDigits :: Int -> [Int]
leDigits n
  | n < 10    = [n]
  | otherwise = case divMod n 10 of
                  (a,b) -> b : leDigits a

valid :: Int -> (Bool,Bool)
valid i = (part1, part2)
  where
  ds         = leDigits i
  lenOK      = length ds == 6
  increasing = and (zipWith (>=) ds (tail ds))
  sames      = filter (>= 2) (map length (group ds))
  basics     = lenOK && increasing
  part1      = basics && not (null sames)
  part2      = basics && not (null (filter (==2) sames))




