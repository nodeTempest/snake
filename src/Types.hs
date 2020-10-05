module Types where

import System.Random

type Coords = (Int, Int)

type SnakeBody = [Coords]

type Food = Coords

data Direction = GoRight | GoDown | GoLeft | GoUp

data GameState = GameState
  { _snakeBody :: SnakeBody,
    _timePassed :: Float,
    _direction :: Direction,
    _food :: Food,
    _rng :: StdGen,
    _paused :: Bool
  }