module Sudoku.GUI where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Sudoku.GUI.State
import qualified Sudoku.GUI.Events as Events

initialState = State { stage = "menu", variant = "", mx = -1, my = -1 }

main = installEventHandler "Sudoku Solver" handleEvents initialState Blank 25

handleEvents :: State -> Input -> (State, [Output])
handleEvents s e = Events.handleEvents s e