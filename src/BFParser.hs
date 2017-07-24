module BFParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.State
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

bfRun :: String -> IO BF
bfRun cmd = snd <$> (bfRun' cmd `runStateT` bfInit)
    where
        bfRun' :: String -> StateT BF IO ()
        bfRun' []  = StateT $ \bf -> return ((),bf)
        bfRun' (cmd : xs) = StateT $ \bf -> do {((),newBF) <- bfDo cmd `runStateT` bf; bfRun' xs `runStateT` newBF}

bfDo :: Char -> StateT BF IO ()
bfDo cmd = do
    bf <- get
    case cmd of
        '+' -> put $ bfInc bf
        '-' -> put $ bfDec bf
        '.' -> do{newBF <- lift $ bfPrint bf; put newBF}


bfInc :: BF -> BF
bfInc bf = bf { register = localMap (+1) (pointer bf) (register bf) }

bfDec :: BF -> BF
bfDec bf = bf { register = localMap (subtract 1) (pointer bf) (register bf) }

chrToStr :: Char -> String
chrToStr chr = [chr]

bfPrint :: BF -> IO BF
bfPrint bf = do
 putStr . chrToStr . chr $ (register bf) !! (pointer bf)
 return bf

localMap :: Num a => (a -> a) -> Int -> [a] -> [a]
localMap f pos list = (take pos list) ++ f (list !! pos) : drop (pos + 1) list
