module P11 where

import Data.Map(Map)
import qualified Data.Map(Map)
import Control.Concurrent
import VM

main :: String -> IO ()
main txt =
  do mem <- parseProgram txt
     undefined
