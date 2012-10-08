module Sudoku.GUI.State (State (..)) where

import Prelude
import Sudoku

data State = State { stage :: String, dim :: Int, mousePressed :: Bool, sudoku :: Sudoku }
