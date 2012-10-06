module Sudoku.GUI.Solver (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn

handleEvents s e = (s, [])

compose :: State -> Picture
compose s = Pictures
  [ composeBackground
  , Translate (-41) (-10) $ Color (makeColor 0.8 0.8 0.8 1) $ Scale 0.2 0.2 $ Text $ variant s
  ]

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.2 0.2 0.2 1) $ rectangleSolid 800 800
  ]
