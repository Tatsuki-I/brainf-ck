module Main where

import System.Environment
import BFParser

main = do
        args <- getArgs
        bfRun $ head args
        putStrLn ""
