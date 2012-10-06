module Sudoku.GUI where

import Prelude
import FPPrac.Graphics
import FPPrac.Events

import Graphics.Gloss

import Sudoku.GUI.State
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver

initialState = State { stage = "menu", variant = "" }

main = installEventHandler "Sudoku Solver" handleEvents initialState (Menu.compose initialState) 25

handleEvents :: State -> Input -> (State, [Output])
handleEvents s e
  | (stage s) == "menu"   = Menu.handleEvents s e
  | (stage s) == "solver" = Solver.handleEvents s e  
  | otherwise             = (s, [])