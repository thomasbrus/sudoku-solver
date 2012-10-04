module Sudoku.Strategy.NakedSimple where

import Data.Char
import Data.List
import Sudoku
import Sudoku.Strategy

exampleSudoku =
  [ "1.......6"
  , "..6.2.7.."
  , "78945.1.3"
  , "...8.7..4"
  , "....3...."
  , ".9...42.1"
  , "31297..4."
  , ".4..12.78"
  , "9.8......" ]

apply :: Sudoku -> Int -> Int -> Char
apply rs i j  | length cs == 1  = head cs
              | otherwise       = rs !! i !! j
              where
                cs = findCandidates rs i j

run :: Sudoku -> Sudoku
run rs  | outcome /= rs = run outcome
        | otherwise     = outcome
        where
          outcome = map (\i -> (map (\j -> apply rs i j) [0..8])) [0..8]
