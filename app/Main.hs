module Main where

import System.Environment
import BFParser

main = do
        args <- getArgs
        x <- bfRun $ args !! 0
        print $ take 5 (register x)
