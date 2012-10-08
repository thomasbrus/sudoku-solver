{-# LANGUAGE RecordWildCards #-}

module Sudoku.GUI.Events (handleEvents) where

import Prelude
import Data.Maybe
import FPPrac.Events
import FPPrac.Graphics hiding (dim)
import Sudoku
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver
import qualified Sudoku.GUI.Raster as Raster

handleEvents state@(State {stage="menu",..}) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = f $ state { stage = "solver", sudoku = empty4x4Sudoku, dim = 4, mousePressed = False }
  | Btn.inBoundary mx my (Menu.buttons !! 1)
  = f $ state { stage = "solver", sudoku = exampleSudoku1, dim = 9, mousePressed = False }
  | Btn.inBoundary mx my (Menu.buttons !! 2)
  = f $ state { stage = "solver", sudoku = empty12x12Sudoku, dim = 12, mousePressed = False }
  where
    f s = (s, redraw s $ MouseUp (mx, my))

handleEvents state@(State {stage="solver",dim=d,sudoku=su,..}) (MouseUp (mx, my))
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = f $ state { stage = "menu", mousePressed = False, invalidCell = Nothing }
  | Btn.inBoundary mx my (Solver.buttons !! 1)
  = f $ state { sudoku = (es d) }
  | isJust cell && not (isTaken su (fst $ fromJust cell) (snd $ fromJust cell))
  = (state { selectedCell = cell, mousePressed = False, invalidCell = Nothing }, [GraphPrompt ("Enter a number", "Range (0..9)")])
  where
    cell = Raster.calculateCell mx my d
    f s = (s, redraw s $ MouseUp (mx, my))
    es d  | d == 4    = empty4x4Sudoku
          | d == 9    = empty9x9Sudoku
          | d == 12   = empty12x12Sudoku

handleEvents state@(State {stage="solver",selectedCell=sc,sudoku=su,dim=d,..}) (Prompt ("Enter a number", n))
  = (s', redraw s' NoInput)
  where
    c = head n
    (row, column) = fromJust sc
    su' | isNothing sc ||
        not (isAllowed su row column c) ||
        notElem [c] (map show [1..d])
        = su
        | isJust sc
        = (updateCell (su) c (fst $ fromJust sc) (snd $ fromJust sc))
    s'  | su == su'
        = state { selectedCell = Nothing, sudoku = su', invalidCell = sc }
        | otherwise
        = state { selectedCell = Nothing, sudoku = su' }

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



