module Main(main) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(unfoldr,mapAccumL)
import Data.Maybe(listToMaybe)

main :: IO ()
main =
  do inp <- parseInput "inputs/7.txt"
     let a = part1 inp
         b = part2 inp
     print (a,b)

type Graph = Map Char (Set Char)

part1 :: Graph -> [Char]
part1 = unfoldr getTask

data State = State
  { tasksTodo :: Graph        -- ^ Tasks that are not finished
  , inProgress :: Set Char
  , workers   :: [Worker]
  }

data Worker = Idle | Working Char Int -- how much work is left for task

initialState :: Graph -> Int -> State
initialState g n = State
  { tasksTodo = g
  , inProgress = Set.empty
  , workers = replicate n Idle
  }

finished :: State -> Bool
finished s = Map.null (tasksTodo s)

chooseTask :: State -> Maybe Char
chooseTask s = listToMaybe $ Map.keys $ Map.filterWithKey available $ tasksTodo s
  where
  available t deps = Set.null deps && not (t `Set.member` inProgress s)

assignTask :: State -> Worker -> (State,Worker)
assignTask s w =
  case w of
    Idle -> case chooseTask s of
              Nothing -> (s,Idle)
              Just t  -> (s { inProgress = Set.insert t (inProgress s) }
                         , Working t (duration t))
    Working {} -> (s,w)

duration :: Char -> Int
duration t = 60 + (fromEnum t - fromEnum 'A')

doWork :: State -> Worker -> (State,Worker)
doWork s w =
  case w of
    Idle -> (s,w)
    Working t n
      | n > 0     -> (s,Working t (n-1))
      | otherwise -> (s { tasksTodo = rmTask t (tasksTodo s)
                        , inProgress = Set.delete t (inProgress s)
                        }, Idle)

withWorkers :: (State -> Worker -> (State,Worker)) -> State -> State
withWorkers f s = let (s1,ws) = mapAccumL f s (workers s)
                  in s1 { workers = ws }


part2 :: Graph -> Int
part2 inp = length steps
  where
  steps = takeWhile (not . finished)
        $ iterate step (initialState inp 5)
  step  = withWorkers doWork . withWorkers assignTask

getTask :: Graph -> Maybe (Char, Graph)
getTask g = do a <- selectTask g
               pure (a, rmTask a g)

selectTask :: Graph -> Maybe Char
selectTask g
  | null g    = Nothing
  | otherwise = Just $ head $ Map.keys $ Map.filter Set.null g

rmTask :: Char -> Graph -> Graph
rmTask n g = fmap (Set.delete n) (Map.delete n g)


--------------------------------------------------------------------------------

graph :: [(Char,Char)] -> Graph
graph xs = Map.fromListWith Set.union
         $ concat [ [ (y,Set.singleton x), (x, Set.empty) ] | (x,y) <- xs ]

parseInput :: FilePath -> IO Graph
parseInput file =
  do txt <- readFile file
     return $ graph $ map getDep $ lines txt
  where
  getDep l = let ws = words l
                 at x = head (ws !! x)
             in (at 1, at 7)
