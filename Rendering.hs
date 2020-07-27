module Rendering where
import Data.List
import Graphics.Gloss
import Game
--Modify width and height to 400
width, height, offset :: Int
width = 400
height = 400 
offset = 100
manColor = red

drawMan :: Game -> Picture
drawMan Game{man = man,answer = Answer ans,gameState = _ ,word =  word} = pictures $ take (fromEnum man + 2)
  [ textDraw $ displayWord word ans
  , headDraw
  , bodyDraw
  , leftArmDraw
  , rightArmDraw
  , leftLegDraw
  , rightLegDraw
  ]

drawWin :: String -> Picture
drawWin word = pictures 
  [ translate (-(fromIntegral width/ 2) + 85) (0) $ Scale 0.40 0.40 $ color manColor $ Text "You Win!"
  , drawReplay 
  , textDraw word]

drawLose :: String -> Picture
drawLose word = pictures 
  [ translate (-(fromIntegral width/ 2) + 70) (0) $ Scale 0.40 0.40 $ color manColor $ Text "You Lose!"
  , drawReplay
  , textDraw word]

headDraw :: Picture
headDraw = translate (0) (100) $ color manColor $ circleSolid 30 

bodyDraw :: Picture
bodyDraw = translate (0) 30 $ rotate 180 $ color manColor $ rectangleSolid 10 100

leftArmDraw :: Picture
leftArmDraw = translate (-15) (30) $ rotate 270 $ color manColor $ rectangleSolid 10 30

rightArmDraw :: Picture
rightArmDraw = translate (15) (30) $ rotate 90 $ color manColor $ rectangleSolid 10 30

leftLegDraw :: Picture
leftLegDraw = translate (-15) (-40) $ rotate 210 $ color manColor $ rectangleSolid 10 75

rightLegDraw :: Picture
rightLegDraw = translate (15) (-40) $ rotate 150 $ color manColor $ rectangleSolid 10 75

drawReplay :: Picture
drawReplay = translate (-180) (174) $ Scale 0.15 0.15 $ color manColor $ Text "Press F2 to replay"

textDraw :: String -> Picture
textDraw x =
  let
    len = length x
  in
    translate (-(fromIntegral width/ 2) + 20) (-120) $ Scale 0.15 0.15 $ color manColor $ Text x

displayWord :: String -> String -> String
displayWord word ans = intersperse ' ' (zipWith (\x y -> if x /= y then x else '_') word ans)

renderGame :: Game -> Picture
renderGame game = case gameState game of
  Running ->  drawMan game
  Win ->  drawWin $ word game
  Lose -> drawLose $ word game
