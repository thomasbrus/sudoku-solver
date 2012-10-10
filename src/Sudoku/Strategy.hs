module Sudoku.Strategy where

import Sudoku
import Prelude
import Data.Char
import Data.Maybe
import Data.List

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates su i j | isTaken su i j  = ""
                      | otherwise       = filter ((isAllowed') su i j) (allowedChars su)

-- TODO first nakedsingle, then nakedpairs
-- TODO implement hiddensingle
step :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
step su f | isNothing m = su
          | isJust m    = do
            let m' = fromJust m
            let i = div m' (rowCount su)
            let j = mod m' (columnCount su)
            let (_, cs) = rcs !! m'
            update su i j (head cs)
          where
            rcs = concat (f su)
            m = findIndex (\(s, cs) -> length cs == 1) rcs

run :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
run su f  | outcome /= su = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f su)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


