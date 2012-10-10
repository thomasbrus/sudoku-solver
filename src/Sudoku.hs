module Sudoku where

import Prelude
import Data.Char
import Data.List

type Sudoku = [[Char]]

rowCount :: Sudoku -> Int
rowCount su = length su

columnCount :: Sudoku -> Int
columnCount su = length $ head su

isTaken :: Sudoku -> Int -> Int -> Bool
isTaken su i j = isDigit $ su !! i !! j

isInRow :: Sudoku -> Int -> Char -> Bool
isInRow su i s = elem s (su !! i)

blockHeight :: Sudoku -> Int
blockHeight su = floor $ sqrt $ fromIntegral $ rowCount su

blockWidth :: Sudoku -> Int
blockWidth su = ceiling $ sqrt $ fromIntegral $ columnCount su

findBlock :: Sudoku -> Int -> Int -> [[Char]]
findBlock su i j  = map ((take w) . (drop j')) (take h $ drop i' su)
                  where
                    h  = blockHeight su
                    w  = blockWidth su
                    i' = (div i h) * h
                    j' = (div j w) * w

isInBlock :: Sudoku -> Int -> Int -> Char -> Bool
isInBlock su i j c  = let block = concat $ findBlock su i j in
                      elem c block

isAllowed :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed su i j c  = isValidChar su c &&                      
                      not (isInRow su i c) &&
                      not (isInRow (transpose su) j c) &&
                      not (isInBlock su i j c)

isAllowed' :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed' su i j c = isAllowed su i j c && not (isTaken su i j)

allowedChars :: Sudoku -> String
allowedChars su = take (columnCount su) "123456789ABCDEFG"

isValidChar :: Sudoku -> Char -> Bool
isValidChar su c = elem c (allowedChars su)

isValid :: Sudoku -> Bool
isValid su  = all (==True) $ concat (mapWithIndeces su f)
            where
              su' i j = update su i j '.'
              f i j   = isAllowed' (su' i j) i j (su !! i !! j)

update :: Sudoku -> Int -> Int -> Char -> Sudoku
update su i j c = mapWithIndeces su f
                where f i' j' | i' == i && j == j' = c
                              | otherwise = su !! i' !! j'

mapWithIndeces :: Sudoku -> (Int -> Int -> b) -> [[b]]
mapWithIndeces su f = let (r, c) = (rowCount su - 1, columnCount su - 1) in
                      map (\i -> (map (f i) [0..c])) [0..r]

prettyprint :: Sudoku -> IO ()
prettyprint su = mapM_ putStrLn su

