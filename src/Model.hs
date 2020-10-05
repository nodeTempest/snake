module Model where

import Config
import Control.Monad.State.Lazy
import System.Random
import Types

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

move :: Direction -> SnakeBody -> SnakeBody
move direction snakeBody =
  let (x, y) = head snakeBody
      newHead = case direction of
        GoRight -> (x + 1, y)
        GoDown -> (x, y + 1)
        GoLeft -> (x - 1, y)
        GoUp -> (x, y - 1)
      collideHead = case direction of
        GoRight -> (0, y)
        GoDown -> (x, 0)
        GoLeft -> (cols - 1, y)
        GoUp -> (x, rows - 1)
      actualHead = if collidesWall newHead then collideHead else newHead
   in actualHead : init snakeBody

generateFood :: StdGen -> SnakeBody -> (Food, StdGen)
generateFood rng snakeBody =
  let rngState = do
        x <- state $ randomR (0, cols - 1)
        y <- state $ randomR (0, rows - 1)
        return (x, y)
      (food, rng') = runState rngState rng
      collidesSnakeBody = food `elem` snakeBody
   in if collidesSnakeBody then generateFood rng' snakeBody else (food, rng')