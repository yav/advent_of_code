import Data.Maybe(mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List(mapAccumL)
import Control.Monad(guard,msum)
import Text.ParserCombinators.ReadP

import Text.Show.Pretty

main :: IO ()
main =
  do txt <- readFile "inputs/20.txt"
     let path      = parseInput txt
         here      = Loc 0 0
         (graph,_) = simulate addGraph path (startGraph here) here
         info      = Map.elems (distances graph here)
     print $ maximum info
     print $ length $ filter (>=1000) info

data Dir  = N | E | S | W deriving (Show,Eq,Ord)
data Path = Go Dir | Path :-> Path | Or Path Path | Nop
            deriving Show

cat :: Path -> Path -> Path
cat p1 p2 = case (p1,p2) of
              (Nop,_)  -> p2
              (_,Nop)  -> p1
              _        -> p1 :-> p2

data Loc = Loc !Int !Int -- y, x
            deriving (Eq,Ord,Show)

move :: Dir -> Loc -> Loc
move d (Loc y x) =
  case d of
    N -> Loc (y-1) x
    E -> Loc y (x+1)
    S -> Loc (y+1) x
    W -> Loc y (x-1)

-- | Returns a map of shortest distances from the original starting point
distances :: Graph -> Loc -> Map Loc Integer
distances g l0 = go (Set.toList (g Map.! l0)) [] (Map.singleton l0 0)
  where
  go todo1 todo2 known =
    case todo1 of
      [] -> case todo2 of
              [] -> known
              _  -> go (reverse todo2) [] known
      x : xs ->
       case Map.lookup x known of
         Just _ -> go xs todo2 known
         Nothing ->
            let next = Set.toList (Map.findWithDefault Set.empty x g)
                d    = 1 + minimum (mapMaybe (`Map.lookup` known) next)
            in go xs (next ++ todo2) (Map.insert x d known)

-- | For each location, keep track of its neighbours
type Graph = Map Loc (Set Loc)

startGraph :: Loc -> Graph
startGraph l = Map.singleton l Set.empty

addGraph :: Loc -> Loc -> Graph -> Graph
addGraph l1 l2 info = add l2 l1 (add l1 l2 info)
  where
  add x y = Map.insertWith Set.union x (Set.singleton y)

simulate :: (Loc -> Loc -> a -> a) -> Path -> a -> Loc -> (a, Set Loc)
simulate step path doors loc =
  case path of

    Go d -> (newDoors, Set.singleton newLoc)
      where
      newLoc    = move d loc
      newDoors  = step loc newLoc doors

    Nop  -> (doors, newLoc)
      where
      newLoc = Set.singleton loc

    Or p1 p2 -> (doors2, newLoc)
      where
      newLoc        = Set.union ls1 ls2
      (doors1,ls1)  = simulate step p1 doors loc
      (doors2,ls2)  = simulate step p2 doors1 loc

    p1 :-> p2 -> (doors2, newLoc)
      where
      newLoc            = Set.unions newLocs
      (doors1,ls1)      = simulate step p1 doors loc
      (doors2,newLocs)  = mapAccumL (simulate step p2) doors1 (Set.toList ls1)

--------------------------------------------------------------------------------

parseInput :: String -> Path
parseInput txt =
  case readP_to_S (between (char '^') (char '$') pPath) txt of
    [ (a,_) ] -> a
    [] -> error "Parse error"
    _ -> error "Ambig"

pPath :: ReadP Path
pPath = foldr1 Or <$> pAtoms `sepBy1` char '|'

pDir :: ReadP Dir
pDir = choice (map tryDir [ N, E, S, W ])
  where
  tryDir x = x <$ string (show x)

pAtom :: ReadP Path
pAtom = choice [ Go <$> pDir, between (char '(') (char ')') pPath ]

pAtoms :: ReadP Path
pAtoms = foldr cat Nop <$> many pAtom

