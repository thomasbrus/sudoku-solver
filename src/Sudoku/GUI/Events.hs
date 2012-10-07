module Sudoku.GUI.Events (handleEvents) where

import Prelude
import FPPrac.Events
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver

handleEvents (State "menu" _ _ _) (MouseDown (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = ((State "solver" "4 x 4" mx my), [])
  | Btn.inBoundary mx my (Menu.buttons !! 1)
  = ((State "solver" "9 x 9" mx my), [])
  | Btn.inBoundary mx my (Menu.buttons !! 2)
  = ((State "solver" "12 x 12" mx my), [])

handleEvents (State "solver" _ _ _) (MouseDown (mx, my)) 
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = ((State "menu" "" mx my), [])

handleEvents s (MouseUp (_, _))
  = (s, [DrawPicture $ composerFromState s])

handleEvents s (MouseMotion (mx, my))
  = (s', [DrawPicture $ composerFromState s ])
  where
    s' = s { mx = mx, my = my }

handleEvents s _ = (s, [])

composerFromState :: State -> Picture
composerFromState s
  | stage s == "menu"
  = Menu.compose s
  | stage s == "solver"
  = Solver.compose s


