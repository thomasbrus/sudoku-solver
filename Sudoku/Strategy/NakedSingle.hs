module Sudoku.Strategy.NakedSimple where

import Data.Char
import Data.List
import Sudoku
import Sudoku.Strategy

run :: Sudoku -> Sudoku
run rs  | outcome /= rs = run outcome
        | otherwise     = outcome
        where
          cm      = toCandidatesMap rs
          outcome = map (\r -> (map resolve r)) cm
          resolve (s, cs) | length cs == 1  = head cs
                          | otherwise       = s
          