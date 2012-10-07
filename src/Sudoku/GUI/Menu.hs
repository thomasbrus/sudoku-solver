module Sudoku.GUI.Menu (compose, buttons) where

import Prelude
import FPPrac.Events (Input)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn

buttons =
  [ Btn.Rectangular (-180, -90, 130, 130) "4 x 4" (84, 21) green
  , Btn.Rectangular (0, -90, 130, 130) "9 x 9" (81, 21) yellow
  , Btn.Rectangular (180, -90, 130, 130) "12 x 12" (108, 21) red
  ]

compose :: State -> Input -> Picture
compose s e = Pictures [composeBackground, Btn.composeAll s e buttons]

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  , Translate (-263) 130 $ Color white $ Scale 0.6 0.6 $ Text "Sudoku Solver" -- 525.6 x 62.4
  , Translate (-152) 78 $ Color (makeColor 0.5 0.5 0.5 1) $ Scale 0.2 0.2 $ Text "Choose a variant below:" -- 304 x 20
  ]

