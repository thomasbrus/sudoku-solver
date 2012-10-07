module Sudoku.GUI.Events (handleEvents) where

import Prelude
import FPPrac.Events
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver

handleEvents (State "menu" _ _) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = (s' 4, redraw (s' 4) $ MouseUp (mx, my))
  | Btn.inBoundary mx my (Menu.buttons !! 1)
  = (s' 9, redraw (s' 9) $ MouseUp (mx, my))
  | Btn.inBoundary mx my (Menu.buttons !! 2)
  = (s' 12, redraw (s' 12) $ MouseUp (mx, my))
  where
    s' d = State "solver" d False

handleEvents (State "solver" d _) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = (s' d, redraw (s' d) $ MouseUp (mx, my))
  where
    s' d = State "menu" d False

handleEvents s (MouseMotion (mx, my))
  = (s, redraw s $ MouseMotion (mx, my))

handleEvents s (MouseDown (mx, my)) =
  let s' = s { mousePressed = True }
  in (s', redraw s' $ MouseDown (mx, my))

handleEvents s (MouseUp (mx, my)) =
  let s' = s { mousePressed = False }
  in (s', redraw s' $ MouseUp (mx, my))

handleEvents s _ = (s, [])

redraw :: State -> Input -> [Output]
redraw s e
  | stage s == "menu"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Menu.compose s e]
  | stage s == "solver"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Solver.compose s e]



