{-# Language RecordWildCards #-}
import Data.Char(isAlphaNum)
import Data.Maybe(mapMaybe)
import Text.Read(readMaybe)
import Control.Monad(zipWithM)
import Data.List(sortBy,maximumBy,foldl')
import Data.Function(on)
import Data.Map(Map)
import qualified Data.Map as Map



main :: IO ()
main =
  do inp <- parseInput "inputs/4.txt"
     let sim = simulate inp
     let a = part1 sim
         b = part2 sim
     print (a,b)


data GuardState = Awake | SleepingSince Time
data State1     = State1 { curGuard :: Maybe (Int,GuardState)
                         , slept    :: Map Int (Map Int Int)
                                      -- ^ | GuardId -> Minute -> Sleeps
                         }

part1 :: [(Int,Map Int Int)] -> Int 
part1 info =
  case totals of
    [] -> error "No events"
    _  -> let (g,(_,m)) = maximumBy (compare `on` (fst . snd)) totals
          in g * m
  where
  summaize (g,mp) = (g, (sum mp, mostMins mp))
  totals          = map summaize info

  -- Find out which miniute got the most sleeping.
  -- If there are none, we return 0;  if there are multiple, return first.
  mostMins :: Map Int Int -> Int
  mostMins mp = case Map.toList mp of
                  [] -> 0
                  xs -> fst (maximumBy (compare `on` snd) xs)



part2 :: [(Int,Map Int Int)] -> Int
part2 info =
  case totals of
    [] -> error "No one slept"
    _  -> let (g,(m,_)) = maximumBy (compare `on` (snd . snd)) totals
          in g * m
  where
  totals = mapMaybe mostCommont info

  mostCommont :: (Int,Map Int Int) -> Maybe (Int,(Int,Int))
  mostCommont (g,mp) = case Map.toList mp of
                        [] -> Nothing
                        xs -> Just (g, maximumBy (compare `on` snd) xs)




simulate :: [Entry] -> [(Int,Map Int Int)]
simulate evs = Map.toList $ slept $ foldl' doEvent s0 evs
  where

  s0 = State1 { curGuard = Nothing, slept = Map.empty }

  doEvent s ev =
    case event ev of
      StartShift n -> s { curGuard = Just (n,Awake) }
      FallAsleep ->
        case curGuard s of
          Just (g,Awake) -> s { curGuard = Just (g,SleepingSince (time ev)) }
          _ -> error "There is awake guard"

      WakeUp ->
       case curGuard s of
         Just (g,SleepingSince t) ->
           State1
            { curGuard = Just (g,Awake)
            , slept    = addSleep g (minute t) (minute (time ev) - 1) (slept s)
            }
         _ -> error "There is no sleeping guard"

  -- Create a map of sleeping minutes (from--to, included)
  sleepTime :: Int -> Int -> Map Int Int
  sleepTime x y = Map.fromList (zip [ x .. y ] (repeat 1))

  -- Record that a guard was sleeping
  addSleep g x y = Map.insertWith (Map.unionWith (+)) g (sleepTime x y)





data Event  = StartShift Int | FallAsleep | WakeUp
              deriving Show

data Time   = Time { year, month, day :: Int, hour, minute :: Int }
              deriving (Eq,Ord,Show)

sameDay :: Time -> Time -> Bool
sameDay x y = same year && same month && same day
  where same f = f x == f y


data Entry  = Entry { time :: Time, event :: Event }
              deriving Show

parseInput :: FilePath -> IO [Entry]
parseInput file =
  do txt <- readFile file
     es  <- zipWithM parseEntry [1..] (lines txt)
     return (sortBy (compare `on` time) es)

parseEntry :: Int -> String -> IO Entry
parseEntry n xs =
  case words (map cleanUp xs) of
    yr : mt : dy : hr : mi : w1 : w2 : _
      | Just year   <- readMaybe yr
      , Just month  <- readMaybe mt
      , Just day    <- readMaybe dy
      , Just hour   <- readMaybe hr
      , Just minute <- readMaybe mi
      , let time = Time { .. }
      , Just event <- case w1 of
                        "falls" -> Just FallAsleep
                        "wakes" -> Just WakeUp
                        "Guard" -> StartShift <$> readMaybe w2
                        _       -> Nothing
        -> pure Entry { .. }

    _ -> fail ("Parse error on line " ++ show n)
  where
  cleanUp c = if isAlphaNum c then c else ' '
