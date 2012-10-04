module Sudoku.Strategy where

import Sudoku
import Data.Char
import Data.List

isTaken :: Sudoku -> Int -> Int -> Bool
isTaken rs i j = isDigit $ rs !! i !! j

isInRow :: Sudoku -> Int -> Char -> Bool
isInRow rs i s = elem s (rs !! i)

findBlock :: Sudoku -> Int -> Int -> [[Char]]
findBlock rs i j  = map ((take 3) . (drop j')) (take 3 $ drop i' rs)
                  where
                    i' = (div i 3) * 3
                    j' = (div j 3) * 3

isInBlock :: Sudoku -> Int -> Int -> Char -> Bool
isInBlock rs i j s  = let symbols = concat $ findBlock rs i j in
                      elem s symbols

isAllowed :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed rs i j s  = not (isTaken rs i j) &&
                      not (isInRow rs i s) &&
                      not (isInRow (transpose rs) j s) &&
                      not (isInBlock rs i j s)

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates rs i j = filter ((isAllowed) rs i j) "123456789"
