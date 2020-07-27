module Game where

import System.Random
import Words
--Data type of the man
data Man = Head | Body | LeftArm | RightArm | LeftLeg | RightLeg | End deriving (Ord, Eq, Enum, Show)

-- A string encapsulated in a data type
data Answer = Answer String deriving (Show, Eq)

-- The different states of execution of the game
data GameState = Running | Win | Lose deriving (Eq,Show)

-- Data type that stores all the relevant information of the game
data Game = Game {man :: Man, answer :: Answer, gameState :: GameState, word :: String} deriving (Eq,Show)

--Advances the state of the man if there was a letter miss
nextPart :: Man -> Man
nextPart x = if (x < End) then succ x else End

--Chooses a random word from a list of words
randWord :: [String] -> Answer
randWord words = 
    let
        gen = mkStdGen 10
        (val', gen') = randomR (1,2) gen :: (Int,StdGen)
    in 
        Answer (words !! val')