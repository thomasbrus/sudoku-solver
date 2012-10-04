module Sudoku.Strategy.NakedSimple where

import Data.Char
import Data.List

type Sudoku = [[Char]]

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
isInBlock rs i j s = elem s symbols
                      where symbols = concat $ findBlock rs i j

isAllowed :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed rs i j s  = not (isTaken rs i j) &&
                      not (isInRow rs i s) &&
                      not (isInRow (transpose rs) j s) &&
                      not (isInBlock rs i j s)

findCandidates :: Sudoku -> Int -> Int -> [Char]
findCandidates rs i j = filter ((isAllowed) rs i j) "123456789"

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

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs

