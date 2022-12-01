import Data.List(sortBy)

main =
  do txt <- readFile "input/01.txt"
     let es = sortBy (flip compare) (elves (lines txt))
     print (head es, sum (take 3 es))

elves ls =
  case ls of
    [] -> []
    _  -> case break null ls of
            (as,bs) -> elf as : elves (drop 1 bs)
  where
  elf = sum . map read
