main =
  do txt <- readFile "input/02.txt"
     let is = map words (lines txt)
     print (sum (map eval is))
     print (sum (map eval2 is))

eval [[they],[me]] = v2 + game
  where
  v1 = 1 + fromEnum they - fromEnum 'A'
  v2 = 1 + fromEnum me   - fromEnum 'X'
  game = case (v1,v2) of
           (1,2) -> 6
           (2,3) -> 6
           (3,1) -> 6
           _ | v1 == v2 -> 3
             | otherwise  -> 0

eval2 [[they],[result]] = 1 + v2 + game
  where
  v1 = fromEnum they - fromEnum 'A'
  (v2,game) =
    case result of
      'X' -> ((v1 - 1) `mod` 3, 0)
      'Y' -> (v1,3)
      _   -> ((v1 + 1) `mod` 3, 6)
