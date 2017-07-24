module Main where

import System.Environment
import BFParser

main = do
        args <- getArgs
        x <- bfRun $ head args
        print $ take 5 (register x)
