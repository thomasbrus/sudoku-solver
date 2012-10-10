module Sudoku.GUI where

import Prelude
import FPPrac.Graphics hiding (dim)
import FPPrac.Events
import Sudoku
import Sudoku.Examples (empty9x9Sudoku)
import Sudoku.GUI.State
import qualified Sudoku.GUI.Events as Events

initialState = State
  { stage = "menu"
  , dim = 9
  , mousePressed = False
  , sudoku = empty9x9Sudoku
  , selectedCell = Nothing
  , invalidCell = Nothing
  }

main = installEventHandler "Sudoku Solver" handleEvents initialState Blank 25

handleEvents :: State -> Input -> (State, [Output])
handleEvents s e = Events.handleEvents s e