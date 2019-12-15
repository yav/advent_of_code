{-# Language BlockArguments #-}
module P11 where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List
import Data.Function(on)
import Control.Concurrent
import Control.Monad
import Data.IORef
import VM


type Pt = (Int,Int)
data R = R { pic :: !(Map Pt Integer)
           , loc :: !Pt
           , dir :: !Dir
           }

data Dir = N | E | S | W deriving (Bounded,Enum,Ord,Eq)

turn :: Int -> Dir -> Dir
turn x = toEnum . (`mod` l) . (+ x) . fromEnum
  where l = fromEnum (maxBound :: Dir) + 1

move :: Dir -> Pt -> Pt
move d (x,y) =
  case d of
    N -> (x,y-1)
    E -> (x+1,y)
    S -> (x,y+1)
    W -> (x-1,y)




main :: String -> IO ()
main txt =
  do mem <- parseProgram txt
     pic1 <- runRobot mem Map.empty
     pic2 <- runRobot mem (Map.singleton (0,0) 1)
     print (length pic1)
     putStrLn (render pic2)

render :: Map Pt Integer -> String
render = unlines
       . doOne "" renderRow
       . groupBy ((==) `on` row)
       . sortBy (compare `on` row)
       . Map.toList
  where
  col ((x,_),_) = x
  row ((_,y),_) = y

  renderRow xs =
    (snd (fst (head xs)), doOne ' ' pix (sortBy (compare `on` col) xs))

  pix ((x,_),p) = (x, if p == 0 then ' ' else '#')

  doOne dflt next xs0 = unfoldr step (0,xs0)
    where
    step (c,xs) =
      case xs of
        [] -> Nothing
        one : more
          | c < x     -> Just (dflt, (c+1,xs))
          | otherwise -> Just (thing, (c+1,more))
             where (x,thing) = next one


runRobot :: Mem -> Map Pt Integer -> IO (Map Pt Integer)
runRobot mem pic0 =
  do ref <- newIORef R { pic = pic0, loc = (0,0), dir = N }
     out <- newEmptyMVar
     wait <- newEmptyMVar
     let getColor = do s <- readIORef ref
                       pure (Map.findWithDefault 0 (loc s) (pic s))

         act f    = do c <- takeMVar out
                       modifyIORef' ref (f c)
                       putMVar wait ()

         setColor = do act \c s -> s { pic = Map.insert (loc s) c (pic s) }
                       act \d s ->
                         let newDir = turn (if d == 0 then (-1) else 1) (dir s)
                             newLoc = move newDir (loc s)
                         in s { dir = newDir, loc = newLoc }

     _ <- forkIO (forever setColor)
     vm <- newVM mem getColor (\v -> putMVar out v >> takeMVar wait)
     runProgram vm
     fin <- readIORef ref
     pure (pic fin)
