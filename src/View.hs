module View where

import Config
import Graphics.Gloss
import Types

toCorner :: Picture -> Picture
toCorner = translate x y
  where
    x = - fromIntegral winWidth / 2 + halfSquare
    y = fromIntegral winHeight / 2 - halfSquare
    halfSquare = fromIntegral squareSize / 2

drawSquare :: Coords -> Picture
drawSquare (x, y) = translate x' y' $ square
  where
    x' = fromIntegral (x * squareSize)
    y' = - fromIntegral (y * squareSize)
    squareSize' = fromIntegral squareSize
    square = rectangleSolid squareSize' squareSize'

drawSnake :: SnakeBody -> Picture
drawSnake snakeBody =
  let snakeTail = map (snakeTailColor . drawSquare) $ tail snakeBody
      snakeHead = snakeHeadColor . drawSquare $ head snakeBody
   in pictures $ snakeHead : snakeTail

drawFood :: Food -> Picture
drawFood = foodColor . drawSquare

render :: GameState -> Picture
render state = toCorner $ pictures [drawSnake (_snakeBody state), drawFood (_food state)]