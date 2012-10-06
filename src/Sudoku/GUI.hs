module Sudoku.GUI where

import Prelude
import FPPrac.Graphics
import FPPrac.Events

import Graphics.Gloss

import qualified Sudoku.GUI.Menu as Menu

data State = State { count :: Int }

initialState = State { count = 0 }

main = installEventHandler "Sudoku Solver" handleEvents initialState Blank 25

handleEvents :: userState -> Input -> (userState, [Output])
handleEvents s (MouseMotion (mx, my)) = (s, [DrawPicture $ Menu.compose mx my])
handleEvents s e = (s, [])
