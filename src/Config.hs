module Config where

import Graphics.Gloss

squareSize :: Int
squareSize = 50

cols :: Int
cols = 12

rows :: Int
rows = 12

speed :: Float
speed = 4

winWidth :: Int
winWidth = cols * squareSize

winHeight :: Int
winHeight = rows * squareSize

snakeHeadColor :: Picture -> Picture
snakeHeadColor = color $ dark red

snakeTailColor :: Picture -> Picture
snakeTailColor = color black

foodColor :: Picture -> Picture
foodColor = color $ dark green