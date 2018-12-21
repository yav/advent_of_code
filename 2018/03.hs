{-# Language RecordWildCards #-}
import Data.Char(isDigit)
import Control.Monad(zipWithM)
import Text.Read(readMaybe)
import qualified Data.Map as Map


main :: IO ()
main =
  do inp <- parseInput "inputs/3.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

part1 :: [Input] -> Int
part1 = Map.size . Map.filter (> 1) . foldr addInput Map.empty
  where
  addSquare x = Map.insertWith (+) x (1 :: Int)
  inpSquares Input{..} = [ (x,y) | x <- take inpW [ inpX .. ]
                                 , y <- take inpH [ inpY .. ]
                                 ]
  addInput i mp = foldr addSquare mp (inpSquares i)

part2 :: [Input] -> [Int]
part2 xs = map inpId (filter check xs)
  where
  check x = all (\y -> inpId y == inpId x || noOverlap x y) xs

  noOverlap i1 i2 = inpX i1 >= (inpX i2 + inpW i2)
                 || inpX i2 >= (inpX i1 + inpW i1)
                 || inpY i1 >= inpY i2 + inpH i2
                 || inpY i2 >= (inpY i1 + inpH i1)


data Input = Input
  { inpId :: Int
  , inpX, inpY :: Int
  , inpW, inpH :: Int
  }

parseInput :: FilePath -> IO [Input]
parseInput file = zipWithM parseLine [1..] =<< (lines <$> readFile file)
  where
  parseLine :: Integer -> String -> IO Input
  parseLine n xs =
    case mapM readMaybe (words [ if isDigit c then c else ' ' | c <- xs ]) of
      Just [ inpId,inpX,inpY,inpW,inpH ] -> pure Input { .. }
      _ -> fail ("Parse error on line " ++ show n)

