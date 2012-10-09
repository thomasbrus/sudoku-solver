module Sudoku.Strategy where

import Sudoku
import Prelude
import Data.Char
import Data.Maybe
import Data.List

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates rs i j | isTaken rs i j  = ""
                      | otherwise       = filter ((isAllowed') rs i j) (allowedChars rs)

--step :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
--step rs f = map (\r -> (map resolve r)) (f rs)
--          where
--            resolve (s, cs) | length cs == 1  = head cs
--                            | otherwise       = s

step :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
step rs f | isNothing m = rs
          | isJust m    = do
            let m' = fromJust m
            let i = div m' (rowCount rs)
            let j = mod m' (columnCount rs)
            let (_, cs) = rcs !! m'
            update rs i j (head cs)
          where
            rcs = concat (f rs)
            m = findIndex (\(s, cs) -> length cs == 1) rcs

run :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
run rs f  | outcome /= rs = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f rs)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


