module P05 where

import Data.List(foldl',sort)

main :: String -> IO ()
main input =
  do print (maximum seats)
     print (search False 0 (sort seats))
  where
  seats = map (seatId . seatNumber) $ lines input

search :: Bool -> Int -> [Int] -> Int
search present expect next =
  case next of
    ~(a : bs)
      | a == expect     -> search True  (expect + 1) bs
      | not present     -> search False (expect + 1) next
      | a == expect + 1 -> expect
      | otherwise       -> search False (expect + 1) next


seatId :: (Int,Int) -> Int
seatId (row,col) = 8 * row + col

seatNumber :: String -> (Int,Int)
seatNumber xs =
  case splitAt 7 xs of
    (row,col) -> (getNumber frontBack row, getNumber leftRight col)

getNumber :: (Char -> Int) -> String -> Int
getNumber decode = packBits . map decode

packBits :: [Int] -> Int
packBits = foldl' addBit 0
  where
  addBit a b = 2 * a + b

frontBack :: Char -> Int
frontBack c = if c == 'F' then 0 else 1

leftRight :: Char -> Int
leftRight c = if c == 'L' then 0 else 1

examples :: [String]
examples =
  [ "BFFFBBFRRR"
  , "FFFBBBFRRR"
  , "BBFFBBFRLL"
  ]
