module Main where

import System.Environment
import System.Process
import BFParser

main = do args <- getArgs
          text <- readFile $ head args
          compile (head args) $ filter (/= '\n') text
          system $ "stack ghc -- -O2 -threaded -o \""
                 ++ take (length (head args) - 3) (head args)
                 ++ "\" \"" ++ head args ++ ".tmp.brainf-ckobj.hs\""
          system $ "rm \"" ++ head args ++ ".tmp.brainf-ckobj.hs\""

        --bfRun $ filter (/= '\n') text
        --putStrLn ""
