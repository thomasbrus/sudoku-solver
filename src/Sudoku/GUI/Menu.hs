module Sudoku.GUI.Menu (compose) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import qualified Sudoku.GUI.Button as Btn

buttons =
  [ Btn.Rectangular (-180, -90, 130, 130) "4 x 4" (84, 21) False green
  , Btn.Rectangular (0, -90, 130, 130) "9 x 9" (81, 21) False yellow
  , Btn.Rectangular (180, -90, 130, 130) "12 x 12" (108, 21) False red
  ]

compose :: Float -> Float -> Picture
compose mx my = Pictures [composeBackground, Pictures $ map Btn.compose buttons]

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  , Translate (-263) 130 $ Color white $ Scale 0.6 0.6 $ Text "Sudoku Solver" -- 525.6 x 62.4
  , Translate (-152) 78 $ Color (makeColor 0.5 0.5 0.5 1) $ Scale 0.2 0.2 $ Text "Choose a variant below:" -- 304 x 20
  ]
