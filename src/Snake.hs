module Snake where

import Config
import Control.Monad.State.Lazy
import Graphics.Gloss
import System.Random

type Coords = (Int, Int)

type SnakeBody = [Coords]

type Food = Coords

type DidConsumeFood = Bool

data Direction = GoRight | GoDown | GoLeft | GoUp

data GameState = GameState
  { _snakeBody :: SnakeBody,
    _timePassed :: Float,
    _direction :: Direction,
    _food :: Food,
    _rng :: StdGen,
    _paused :: Bool
  }

initialState :: GameState
initialState =
  GameState
    { _snakeBody =
        let (x, y) = (rows `div` 2, cols `div` 2)
         in [ (x, y),
              (x - 1, y),
              (x - 2, y)
            ],
      _timePassed = 0,
      _direction = GoRight,
      _food = (8, 8),
      _rng = mkStdGen (0),
      _paused = True
    }

collidesWall :: Coords -> Bool
collidesWall (x, y) =
  let collidesX = not $ x `elem` [0 .. cols - 1]
      collidesY = not $ y `elem` [0 .. rows - 1]
   in collidesX || collidesY

move :: Direction -> SnakeBody -> Food -> (SnakeBody, DidConsumeFood)
move direction snakeBody food =
  let (x, y) = head snakeBody
      movedHead = case direction of
        GoRight -> (x + 1, y)
        GoDown -> (x, y + 1)
        GoLeft -> (x - 1, y)
        GoUp -> (x, y - 1)
      collideHead = case direction of
        GoRight -> (0, y)
        GoDown -> (x, 0)
        GoLeft -> (cols - 1, y)
        GoUp -> (x, rows - 1)
      newHead = if collidesWall movedHead then collideHead else movedHead
      movedSnake = newHead : init snakeBody
      didTrapOnFood = head movedSnake == food
      fedSnake = food : snakeBody
   in if didTrapOnFood
        then (fedSnake, True)
        else (movedSnake, False)

generateFood :: SnakeBody -> StdGen -> (Food, StdGen)
generateFood snakeBody rng =
  let rngState = do
        x <- state $ randomR (0, cols - 1)
        y <- state $ randomR (0, rows - 1)
        return (x, y)
      (food, rng') = runState rngState rng
      collidesSnakeBody = food `elem` snakeBody
   in if collidesSnakeBody then generateFood snakeBody rng' else (food, rng')

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
  let snakeHead = snakeHeadColor . drawSquare $ head snakeBody
      snakeTail = map (snakeTailColor . drawSquare) $ tail snakeBody
   in pictures $ snakeHead : snakeTail

drawFood :: Food -> Picture
drawFood = foodColor . drawSquare
