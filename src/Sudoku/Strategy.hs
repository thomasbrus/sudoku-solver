module Sudoku.Strategy where

import Sudoku
import Prelude
import Data.Char

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates rs i j | isTaken rs i j  = ""
                      | otherwise       = filter ((isAllowed) rs i j) "123456789"

run :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
run rs f  | outcome /= rs = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f rs)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


