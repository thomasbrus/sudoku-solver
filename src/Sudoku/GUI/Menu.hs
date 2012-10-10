module Sudoku.GUI.Menu (draw, buttons) where

import Prelude
import FPPrac.Events (Input)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn

buttons :: [Btn.Button]
buttons =
  [ Btn.Rectangular (-180, -90, 130, 130) "4 x 4" (84, 21) green
  , Btn.Rectangular (0, -90, 130, 130) "6 x 6" (81, 21) yellow
  , Btn.Rectangular (180, -90, 130, 130) "9 x 9" (81, 21) red
  ]

draw :: State -> Input -> Picture
draw s e = Pictures [drawBackground, Btn.drawAll s e buttons]

drawBackground :: Picture
drawBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  , Translate (-263) 130 $ Color white $ Scale 0.6 0.6 $ Text "Sudoku Solver" -- 525.6 x 62.4
  , Translate (-152) 78 $ Color (makeColor 0.5 0.5 0.5 1) $ Scale 0.2 0.2 $ Text "Choose a variant below:" -- 304 x 20
  ]
