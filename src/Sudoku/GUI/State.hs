module Sudoku.GUI.State (State (..)) where

import Prelude

data State = State { stage :: String, variant :: String, mousePressed :: Bool }
