module Main where

import System.Environment
import BFParser

main = do
        args <- getArgs
        text <- readFile $ head args
        bfRun $ filter (/= '\n') text
        putStrLn ""
