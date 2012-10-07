module Sudoku.GUI.State (State (..)) where

import Prelude

data State = State { stage :: String, dim :: Int, mousePressed :: Bool }
