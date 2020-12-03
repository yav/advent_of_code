module P02 where

import Data.Bits(xor)

main :: String -> IO ()
main txt = solve validRecord1 >> solve validRecord2
  where
  input = parseInput txt
  solve validate = print $ length $ filter validate input

data Record = Record
  { atLeast, atMost :: Int
  , letter :: Char
  , password :: String
  } deriving Show

parseInput :: String -> [Record]
parseInput = map parseRecord . lines

parseRecord :: String -> Record
parseRecord xs =
  head
  do (atLeast,'-' : rest) <- reads xs
     (atMost, ' ' : letter : ':' : ' ' : password) <- reads rest
     pure Record { .. }

validRecord1 :: Record -> Bool
validRecord1 Record { .. } = atLeast <= occurs && occurs <= atMost
  where
  occurs = length (filter (== letter) password)

validRecord2 :: Record -> Bool
validRecord2 Record { .. } = check atLeast `xor` check atMost
  where
  check n = case drop (n-1) password of
              x : _ -> x == letter
              _ -> False


