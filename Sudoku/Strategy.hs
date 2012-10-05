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
findCandidates rs i j | isTaken rs i j  = ""
                      | otherwise       = filter ((isAllowed) rs i j) "123456789"

mapWithIndeces :: Sudoku -> (Sudoku -> Int -> Int -> b) -> [[b]]
mapWithIndeces rs f = let (rows, columns) = (rowCount rs, columnCount rs) in
                      map (\i -> (map (\j -> f rs i j) [0..(columns - 1)])) [0..(rows - 1)]

run :: Sudoku -> (Sudoku -> [[(Char, String)]]) -> Sudoku
run rs f  | outcome /= rs = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f rs)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


