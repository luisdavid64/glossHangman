import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Hangman
import Rendering
import Words

window :: Display
window = InWindow "Hangman" (width, height) (offset, offset)

background :: Color
background = black

main :: IO()
main = do
    val <- readTextFile
    let initialGame = Game {  man = Head
                    , answer = Answer val
                    , gameState = Running
                    , word = val}
    play window background 30 initialGame renderGame getGuess (\_ -> id)