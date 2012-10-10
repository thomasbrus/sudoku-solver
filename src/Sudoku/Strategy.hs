module Sudoku.Strategy where

import Sudoku
import Prelude
import Data.Maybe
import Data.List

type Solver = (Sudoku -> [[(Char, String)]])

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates su i j | isTaken su i j  = ""
                      | otherwise       = filter ((isAllowed') su i j) (allowedChars su)

findResolvableCell :: Sudoku -> Solver -> Maybe (Int, Int, Char)
findResolvableCell su f
  | isNothing m = Nothing
  | isJust m = let
      m' = fromJust m
      i = div m' (rowCount su)
      j = mod m' (columnCount su)
      (_, cs) = rcs !! m'
    in
      Just (i, j, head cs)
  where
    rcs = concat (f su)
    m   = findIndex (\(_, cs) -> length cs == 1) rcs

step :: Sudoku -> [Solver] -> Sudoku
step su [] = su
step su (f:fs)
  | isNothing r = step su fs
  | isJust r = let (i, j, c) = fromJust r in update su i j c
  where
    r = findResolvableCell su f

run :: Sudoku -> Solver -> Sudoku
run su f  | outcome /= su = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f su)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


