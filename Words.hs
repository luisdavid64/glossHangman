module Words where

import System.IO
import Control.Monad.Random
import Data.Char (ord)
import Dictionary

--Produces a random number from 1 to n-1
randNumber :: (RandomGen g) => Int -> Rand g Int
randNumber n = getRandomR (0,n-1)

--reads the dictionary and yields a random element from it
readTextFile :: IO String
readTextFile = do
    value <- evalRandIO $ (randNumber len)
    return (dictionary !! value)

--Weird function to get a 'random' value for next word
strToInt :: [Char] -> Int
strToInt [] = 1
strToInt (x:x':xs) = (ord x)*(ord x') + strToInt xs
strToInt (x:xs) = (ord x) + strToInt xs

--Produces the next word for the game
newWord :: String -> [String] -> String
newWord old list = list !! ((strToInt old) `mod` len)