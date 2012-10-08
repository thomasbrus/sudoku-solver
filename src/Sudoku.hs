module Sudoku where

import Prelude
import Data.Char
import Data.List

type Sudoku = [[Char]]

exampleSudoku1 =
  [ "1.......6"
  , "..6.2.7.."
  , "78945.1.3"
  , "...8.7..4"
  , "....3...."
  , ".9...42.1"
  , "31297..4."
  , ".4..12.78"
  , "9.8......" ]

exampleSudoku2 =
  [ "4.....938"
  , ".32.941.."
  , ".953..24."
  , "37.6.9..4"
  , "529..1673"
  , "6.47.3.9."
  , "957..83.."
  , "..39..4.."
  , "24..3.7.9" ]

exampleSudoku3 =
  [ ".8..9..3."
  , ".3.....69"
  , "9.2.63158"
  , ".2.8.459."
  , "8519.7.46"
  , "3946.587."
  , "563.4.987"
  , "2......15"
  , ".1..5..2." ]

empty4x4Sudoku =
  [ "...."
  , "...."
  , "...."
  , "...."  
  ]

empty9x9Sudoku =
  [ "........."
  , "........."
  , "........."
  , "........."
  , "........."
  , "........."
  , "........."
  , "........."
  , "........."
  ]

empty12x12Sudoku =
  [ "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  , "............"
  ]

rowCount :: Sudoku -> Int
rowCount rs = length rs

columnCount :: Sudoku -> Int
columnCount rs = length $ head rs

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

updateCell :: Sudoku -> Char -> Int -> Int -> Sudoku
updateCell rs c i j = mapWithIndeces rs f
                    where
                      f _ i' j' | i' == i && j == j' = c
                                | otherwise = rs !! i' !! j'

mapWithIndeces :: Sudoku -> (Sudoku -> Int -> Int -> b) -> [[b]]
mapWithIndeces rs f = let (rows, columns) = (rowCount rs, columnCount rs) in
                      map (\i -> (map (\j -> f rs i j) [0..(columns - 1)])) [0..(rows - 1)]

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs

