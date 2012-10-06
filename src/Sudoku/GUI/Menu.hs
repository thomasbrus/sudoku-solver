module Sudoku.GUI.Menu (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Solver as Solver
import qualified Sudoku.GUI.Button as Btn

buttons =
  [ Btn.Rectangular (-180, -90, 130, 130) "4 x 4" (84, 21) False green
  , Btn.Rectangular (0, -90, 130, 130) "9 x 9" (81, 21) False yellow
  , Btn.Rectangular (180, -90, 130, 130) "12 x 12" (108, 21) False red
  ]

handleEvents s (MouseMotion (mx, my))
  = (s, [DrawOnBuffer False, DrawPicture $ composeButtons s buttons mx my ])

handleEvents s (MouseDown (mx, my)) 
  | Btn.inBoundary mx my (buttons !! 0)
  = (s1, [DrawPicture $ Solver.compose s1])
  | Btn.inBoundary mx my (buttons !! 1)
  = (s2, [DrawPicture $ Solver.compose s2])
  | Btn.inBoundary mx my (buttons !! 2)
  = (s3, [DrawPicture $ Solver.compose s3])
  where
    s1 = State "solver" "4 x 4"
    s2 = State "solver" "9 x 9"
    s3 = State "solver" "12 x 12"

handleEvents s e = (s, [])

compose :: State -> Picture
compose _ = Pictures [composeBackground]

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  , Translate (-263) 130 $ Color white $ Scale 0.6 0.6 $ Text "Sudoku Solver" -- 525.6 x 62.4
  , Translate (-152) 78 $ Color (makeColor 0.5 0.5 0.5 1) $ Scale 0.2 0.2 $ Text "Choose a variant below:" -- 304 x 20
  ]

composeButtons :: State -> [Btn.Button] -> Float -> Float -> Picture
composeButtons s btns mx my = Pictures $ map (Btn.compose s) $ map (Btn.update mx my) btns

