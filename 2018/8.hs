module Main(main) where

main :: IO ()
main =
  do inp <- getInput "inputs/8.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

part1 :: Node -> Int
part1 = sum . meta
  where
  meta (Node xs ms) = xs ++ concatMap meta ms

part2 :: Node -> Int
part2 = eval
  where
  eval (Node xs ms) =
    case ms of
      [] -> sum xs
      _  -> let valOf i = if i < 1 then 0
                            else case splitAt (i-1) ms of
                                   (_,n:_) -> eval n
                                   _ -> 0
            in sum (map valOf xs)


data Node = Node [Int] [Node]

getInput :: FilePath -> IO Node
getInput file =
  do txt <- readFile file
     let nums = map read (words txt)
     case parse nums of
       Just (t,[]) -> pure t
       _ -> fail "Invalid input"

parse :: [Int] -> Maybe (Node,[Int])
parse xs =
  case xs of
    cn : mn : more ->
      do (cs,ys) <- parseNodes cn more
         let (m,rest) = splitAt mn ys
         pure (Node m cs, rest)
    _ -> Nothing

parseNodes :: Int -> [Int] -> Maybe ([Node],[Int])
parseNodes n xs
  | n > 0     = do (t,ys) <- parse xs
                   (ts,zs) <- parseNodes (n-1) ys
                   pure (t:ts,zs)
  | otherwise = Just ([],xs)

