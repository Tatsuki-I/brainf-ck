module BFParser (bfRun) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.State
import Data.Char

data BF = BF { register :: [Int]
             , pointer  ::  Int
             } deriving (Show)

symbol :: Parser Char
symbol =  oneOf "+-<>[],."

readExpr       :: String -> String
readExpr input =  case parse (spaces >> symbol) "brainf*ck" input of
                    Left  err -> "No match:" ++ show err
                    Right val -> "Foudn value"

spaces :: Parser ()
spaces =  skipMany1 space

bfInit :: BF
bfInit =  BF { register = [0, 0 ..]
             , pointer = 0 }

bfRun      :: String -> IO BF
bfRun cmds =  mapM_ bfDo cmds `execStateT` bfInit

bfDo     :: Char -> StateT BF IO ()
bfDo cmd =  case cmd of
              '+' -> modify $ bfReg (+ 1)
              '-' -> modify $ bfReg (subtract 1) 
              '>' -> modify $ bfShift (+ 1)
              '<' -> modify $ bfShift (subtract 1)
              '.' -> get >>= (lift . bfPrint) >>= put
              ',' -> undefined
              '[' -> undefined
              ']' -> undefined
              _   -> undefined

bfReg      :: (Int -> Int) -> BF -> BF
bfReg f bf =  bf { register = localMap f (pointer bf) (register bf) }

bfShift      :: (Int -> Int) -> BF -> BF
bfShift f bf =  bf { pointer = f $ pointer bf}

chrToStr :: Char -> String
chrToStr =  (:[])

bfPrint    :: BF -> IO BF
bfPrint bf =  do putStr . chrToStr . chr $ register bf !! pointer bf
                 return bf

localMap        :: Num a => (a -> a) -> Int -> [a] -> [a]
localMap f p xs =  take p xs ++ f (xs !! p) : drop (p + 1) xs
