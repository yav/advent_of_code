module Main where

import System.FilePath(dropExtension,takeFileName)
import System.Environment(getArgs)

import qualified P01
import qualified P02

main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] ->
          do txt <- readFile f
             case dropExtension (takeFileName f) of
               "01" -> P01.main txt
               "02" -> P02.main txt
               _   -> putStrLn ("I don't know how to solve problem " ++ show f)

       _   -> putStrLn "Need a problem number"


