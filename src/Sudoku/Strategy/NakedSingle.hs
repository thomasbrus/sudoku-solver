module Sudoku.Strategy.NakedSingle where

import Data.Char
import Data.List
import Prelude
import Sudoku
import Sudoku.Strategy

simpleSudoku =
  [ "1.......6"
  , "..6.2.7.."
  , "78945.1.3"
  , "...8.7..4"
  , "....3...."
  , ".9...42.1"
  , "31297..4."
  , ".4..12.78"
  , "9.8......" ]

resolveAllCandidates :: Sudoku -> [[(Char, String)]]
resolveAllCandidates su = mapWithIndeces su (\i j -> (su !! i !! j, findCandidates su i j))

solve :: Sudoku -> Sudoku
solve su = run su resolveAllCandidates
