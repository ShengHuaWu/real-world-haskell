module Main where

import RecursiveContents

main = do
    paths <- getRecursiveContents "/Users/sw819i/Downloads/Temp"
    print paths