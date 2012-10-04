module SimpleSolver where

import Data.Char
import Data.List
import Data.Maybe

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

mapTakenCellCount :: Sudoku -> [Int]
mapTakenCellCount rs  = map (\r -> length $ filter isDigit r) rs

findFullestRowIndex :: Sudoku -> Int
findFullestRowIndex rs  = let x = mapTakenCellCount rs in fromJust $ elemIndex (maximum x) x

findFullestRow :: Sudoku -> [Char]
findFullestRow rs = rs !! (findFullestRowIndex rs)

findMissingNumbers :: [Char] -> [Char]
findMissingNumbers rs = filter (\x -> not $ elem x rs) "123456789"

isTaken :: Sudoku -> Int -> Int -> Bool
isTaken rs i j = isDigit $ rs !! i !! j

isRowTaken :: Sudoku -> Int -> Char -> Bool
isRowTaken rs i d = elem d (rs !! i)

isValid :: Sudoku -> Int -> Int -> Char -> Bool
isValid rs i j d  = not (isTaken rs i j) &&
                    not (isRowTaken rs i d) &&
                    not (isRowTaken (transpose rs) j d)


--n = map (\x -> map (\y -> ) ) $ findMissingNumbers $ findFullestRow exampleSudoku

--mapEmptyCells :: Sudoku -> (a -> a) :: 

