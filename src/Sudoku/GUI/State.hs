module Sudoku.GUI.State (State (..)) where

import Prelude

data State = State { stage :: String, variant :: String,  mx :: Float, my :: Float }
