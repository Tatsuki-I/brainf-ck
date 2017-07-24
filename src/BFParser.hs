module BFParser where

import Text.ParserCombinators.Parsec hiding (spaces)
--import Control.Monad.State
import Data.Char

data BF = BF
        { register :: [Int]
        , pointer  ::  Int
        } deriving (Show)

symbol :: Parser Char
symbol = oneOf "+-<>[],."

readExpr :: String -> String
readExpr input = 
    case parse (spaces >> symbol) "brainf*ck" input of
        Left err -> "No match:" ++ show err
        Right val -> "Foudn value"

spaces :: Parser ()
spaces = skipMany1 space

bfInit ::  BF
bfInit = BF 
        { register = [0, 0 ..]
        , pointer = 0 }

bfRun :: String -> BF
bfRun cmd = bfRun' cmd bfInit
    where
        bfRun' :: String -> BF -> BF
        bfRun' [] bf = bf
        bfRun' (cmd : xs) bf = bfRun' xs (bfDo bf cmd)

bfDo :: BF -> Char -> BF
bfDo bf cmd = case cmd of
        '+' -> bfInc bf
        '-' -> bfDec bf
        '.' -> bfPrint bf


bfInc :: BF -> BF
bfInc bf = bf { register = localMap (+1) (pointer bf) (register bf) }

bfDec :: BF -> BF
bfDec bf = bf { register = localMap (subtract 1) (pointer bf) (register bf) }

chrToStr :: Char -> String
chrToStr chr = [chr]

bfPrint :: BF -> IO ()
bfPrint bf = putStr . chrToStr . chr $ (register bf) !! (pointer bf)

localMap :: Num a => (a -> a) -> Int -> [a] -> [a]
localMap f pos list = (take pos list) ++ f (list !! pos) : drop (pos + 1) list