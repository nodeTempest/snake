module Main where

import Config
import Controller
import Graphics.Gloss
import Model
import View

window :: Display
window = InWindow "Snake" (winWidth, winHeight) (100, 100)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render onKeyPress nextFrame
