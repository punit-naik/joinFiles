module Main where

import Lib
import Data.List.Split (splitOn)
import Data.List (length, drop)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  joinFiles (args !! 0) (args !! 1) (drop 2 args)