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

exampleSudoku4 =
  [ "..4."
  , "1..."
  , "...3"
  , ".1.."  
  ]

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
findBlock rs i j  = map ((take d) . (drop j')) (take d $ drop i' rs)
                  where
                    d  = floor $ sqrt $ fromIntegral $ columnCount rs
                    i' = (div i d) * d
                    j' = (div j d) * d

isInBlock :: Sudoku -> Int -> Int -> Char -> Bool
isInBlock rs i j s  = let block = concat $ findBlock rs i j in
                      elem s block

isAllowed :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed rs i j s  = not (isTaken rs i j) &&
                      not (isInRow rs i s) &&
                      not (isInRow (transpose rs) j s) &&
                      not (isInBlock rs i j s)

allowedChars :: Sudoku -> String
allowedChars su = take (columnCount su) "123456789ABCDEFG"

update :: Sudoku -> Char -> Int -> Int -> Sudoku
update rs c i j = mapWithIndeces rs f
                where f i' j' | i' == i && j == j' = c
                              | otherwise = rs !! i' !! j'

mapWithIndeces :: Sudoku -> (Int -> Int -> b) -> [[b]]
mapWithIndeces rs f = let (r, c) = (rowCount rs - 1, columnCount rs - 1) in
                      map (\i -> (map (f i) [0..c])) [0..r]

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs

