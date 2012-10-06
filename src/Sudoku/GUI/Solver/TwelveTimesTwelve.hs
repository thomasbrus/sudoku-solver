module Sudoku.GUI.Solver.TwelveTimesTwelve (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Raster as Raster
import qualified Sudoku.GUI.Button as Btn

handleEvents s e = (s, [])

compose :: State -> Picture
compose s = Raster.compose s 12