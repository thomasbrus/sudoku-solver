module Sudoku.GUI.Events (handleEvents) where

import Prelude
import FPPrac.Events
import FPPrac.Graphics
import Sudoku
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver

handleEvents (State "menu" _ _ su) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = f $ State "solver" 4 False empty4x4Sudoku
  | Btn.inBoundary mx my (Menu.buttons !! 1)
  = f $ State "solver" 9 False exampleSudoku1
  | Btn.inBoundary mx my (Menu.buttons !! 2)
  = f $ State "solver" 12 False empty12x12Sudoku
  where
    f state = (state, redraw state $ MouseUp (mx, my))

handleEvents (State "solver" d _ su) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = f $ State "menu" d False [""]
  | Btn.inBoundary mx my (Solver.buttons !! 1)
  = f $ State "solver" d False (es d)
  where
    f state = (state, redraw state $ MouseUp (mx, my))
    es d  | d == 4    = empty4x4Sudoku
          | d == 9    = empty9x9Sudoku
          | d == 12   = empty12x12Sudoku

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



