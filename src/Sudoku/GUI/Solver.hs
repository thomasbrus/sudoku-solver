module Sudoku.GUI.Solver (compose, buttons) where

import Prelude
import FPPrac.Events (Input)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Raster as Raster

buttons =
  [ Btn.Rectangular (290, (-240), 190, 60) "Go Back" (108, 21) (greyN 0.5)
  , Btn.Rectangular (290, (-160), 190, 60) "Clear All" (100, 21) (dark red)
  ]

compose :: State -> Input -> Picture
compose s e = Pictures
  [ composeBackground
  , Raster.compose s e
  , Btn.composeAll s e buttons
  ]

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  ]
