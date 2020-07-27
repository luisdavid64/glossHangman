module Hangman where

import Data.Char
import Data.List
import Game
import Words
import Dictionary
import Graphics.Gloss.Interface.Pure.Game

--Take a players guess and compare it to the word
makeGuess :: Game -> Char -> Game
makeGuess Game{man = man,answer = Answer ans,gameState = st,word = word} letter
   | elem letter word = checkState Game{man = man,answer = Answer (fmap (\y -> if (y == letter) || (y == '_') then '_' else y) ans),gameState = st,word =  word}
   | otherwise = checkState Game{man = (nextPart man), answer = Answer ans, gameState = st, word =  word}
--This functions sets the state of the game
checkState :: Game -> Game
checkState Game{man = man, answer = Answer ans, gameState = st, word = word}
    | man == End =  Game{man = man, answer = Answer ans, gameState = Lose, word = word} -- Lose condition 
    | all (== True) (map (== '_') ans) =  Game{man = man, answer = Answer ans, gameState = Win, word = word} --Win condition
    | otherwise = Game{man = man, answer = Answer ans, gameState = st, word = word}

--Input handler
getGuess :: Event -> Game -> Game
getGuess (EventKey (Char input) Down _ _) game
   | isAlpha input =  makeGuess game input
   | otherwise = game
getGuess (EventKey (SpecialKey KeyF2) Down _ _)  Game{word = word} = let new = newWord word dictionary in 
   Game{man = Head, answer = Answer new, gameState = Running, word = new}
getGuess _ game = game