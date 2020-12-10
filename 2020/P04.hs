module P04 where

import Data.List(unfoldr)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(guard,when)

main :: String -> IO ()
main input =
  do countValid False
     countValid True
  where
  db = parseInput input
  countValid strong = print $ length $ filter (valid strong) db

type Passport = Map String String

parseInput :: String -> [Passport]
parseInput = unfoldr nextPassport . lines

nextPassport :: [String] -> Maybe (Passport,[String])
nextPassport ls =
  case dropWhile null ls of
    [] -> Nothing
    _  -> case break null ls of
            (as,bs) -> Just (parsePassport as, drop 1 bs)

parsePassport :: [String] -> Passport
parsePassport = Map.fromList . map parseEntry . concatMap words
  where parseEntry xs = case break (== ':') xs of
                          (as,bs) -> (as,tail bs)

valid :: Bool -> Passport -> Bool
valid strong pass =
  case mapM_ check fields of
    Just _ -> True
    _      -> False
  where
  check (f,p) = do val <- Map.lookup f pass
                   when strong (guard (p val))

  fields = [ ("byr", check4 1920 2002)
           , ("iyr", check4 2010 2020)
           , ("eyr", check4 2020 2030)
           , ("hgt", validHeight)
           , ("hcl", validColor)
           , ("ecl", validEyeColor)
           , ("pid", validPassport)
           ]


isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isHex :: Char -> Bool
isHex x = isDigit x || 'a' <= x && x <= 'f'

check4 :: Int -> Int -> String -> Bool
check4 lower upper xs =
  not $ null
  do guard (length xs == 4)
     (n,"") <- reads xs
     guard (lower <= n && n <= upper)

validColor :: String -> Bool
validColor x =
  case x of
    '#' : xs -> length xs == 6 && all isHex xs
    _ -> False

validEyeColor :: String -> Bool
validEyeColor x = x `elem` [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

validPassport :: String -> Bool
validPassport xs = length xs == 9 && all isDigit xs

validHeight :: String -> Bool
validHeight xs =
  not $ null
  do (n,rest) <- reads xs
     case rest of
       "cm" -> guard (150 <= n && (n::Int) <= 193)
       "in" -> guard (50  <= n && n <= 76)
       _    -> []

example :: String
example = unlines
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  , "byr:1937 iyr:2017 cid:147 hgt:183cm"
  , ""
  , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  , "hcl:#cfa07d byr:1929"
  , ""
  , "hcl:#ae17e1 iyr:2013"
  , "eyr:2024"
  , "ecl:brn pid:760753108 byr:1931"
  , "hgt:179cm"
  , ""
  , "hcl:#cfa07d eyr:2025 pid:166559648"
  , "iyr:2011 ecl:brn hgt:59in"
  ]
