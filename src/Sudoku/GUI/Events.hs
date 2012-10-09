{-# LANGUAGE RecordWildCards #-}

module Sudoku.GUI.Events (handleEvents) where

import Prelude
import Data.Maybe
import FPPrac.Events
import FPPrac.Graphics hiding (dim)
import Sudoku
import Sudoku.Strategy
import Sudoku.Strategy.NakedSingle
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver
import qualified Sudoku.GUI.Raster as Raster

handleEvents state@(State {stage="menu",..}) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = f $ restoreState state { stage = "solver", sudoku = empty4x4Sudoku, dim = 4 }
  | Btn.inBoundary mx my (Menu.buttons !! 1)
  = f $ restoreState state { stage = "solver", sudoku = exampleSudoku1, dim = 9 }
  | Btn.inBoundary mx my (Menu.buttons !! 2)
  = f $ restoreState state { stage = "solver", sudoku = empty12x12Sudoku, dim = 12 }
  where
    f s = (s, redraw s $ MouseUp (mx, my))

handleEvents state@(State {stage="solver",dim=d,sudoku=su,..}) (MouseUp (mx, my))
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = f $ restoreState state { stage = "menu" }
  | Btn.inBoundary mx my (Solver.buttons !! 1)
  = f $ restoreState state { sudoku = (es d) }
  | Btn.inBoundary mx my (Solver.buttons !! 2)
  = f $ restoreState state { sudoku = (ns d) }
  | isJust cell
  = (restoreState state { selectedCell = cell }, [GraphPrompt ("Enter a number", hint)])
  where
    range = allowedChars su
    hint = "Range (" ++ (show $ head range) ++ ".." ++ (show $ last range) ++ ")"
    cell = Raster.calculateCell mx my d
    f s = (s, redraw s $ MouseUp (mx, my))
    ns d = step su resolveAllCandidates
    es d  | d == 4    = empty4x4Sudoku
          | d == 9    = empty9x9Sudoku
          | d == 12   = empty12x12Sudoku

handleEvents state@(State {stage="solver",selectedCell=sc,sudoku=su,dim=d,..}) (Prompt ("Enter a number", n))
  = (s', redraw s' NoInput)
  where
    c = if null n then '.' else head n
    (row, column) = fromJust sc
    su' | c /= '.' &&
          (isNothing sc ||
          not (isAllowed su row column c))
        = su
        | isJust sc
        = (update (su) (fst $ fromJust sc) (snd $ fromJust sc)) c
    s'  | su == su' && c /= '.'
        = state { selectedCell = Nothing, sudoku = su', invalidCell = sc }
        | otherwise
        = state { selectedCell = Nothing, sudoku = su' }

handleEvents s (MouseMotion (mx, my))
  = (s, redraw s $ MouseMotion (mx, my))

handleEvents s (MouseDown (mx, my)) =
  let s' = s { mousePressed = True, invalidCell = Nothing }
  in (s', redraw s' $ MouseDown (mx, my))

handleEvents s (MouseUp (mx, my)) =
  let s' = s { mousePressed = False }
  in (s', redraw s' $ MouseUp (mx, my))

handleEvents s _ = (s, [])


restoreState :: State -> State
restoreState s = s { mousePressed = False }

redraw :: State -> Input -> [Output]
redraw s e
  | stage s == "menu"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Menu.draw s e]
  | stage s == "solver"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Solver.draw s e]

