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

exampleSudoku5 =
  [ "...1..4.7..8"
  , ".4...1...B.2"
  , "..5.A..3.4.."
  , ".24.7B...6C."
  , "A......8.2.5"
  , "...C.6..4.B."
  , ".9.4..6.B..."
  , "7.8.2......1"
  , ".16...5B.32."
  , "..1.3..2.A.."
  , "4.7...A...6."
  , "5..8.4..3..."
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


-- TODO rename rs to su

rowCount :: Sudoku -> Int
rowCount rs = length rs

columnCount :: Sudoku -> Int
columnCount rs = length $ head rs

isTaken :: Sudoku -> Int -> Int -> Bool
isTaken rs i j = isDigit $ rs !! i !! j

isInRow :: Sudoku -> Int -> Char -> Bool
isInRow rs i s = elem s (rs !! i)

blockHeight :: Sudoku -> Int
blockHeight rs = floor $ sqrt $ fromIntegral $ rowCount rs

blockWidth :: Sudoku -> Int
blockWidth rs = ceiling $ sqrt $ fromIntegral $ columnCount rs

findBlock :: Sudoku -> Int -> Int -> [[Char]]
findBlock rs i j  = map ((take w) . (drop j')) (take h $ drop i' rs)
                  where
                    h  = blockHeight rs
                    w  = blockWidth rs
                    i' = (div i h) * h
                    j' = (div j w) * w

isInBlock :: Sudoku -> Int -> Int -> Char -> Bool
isInBlock rs i j c  = let block = concat $ findBlock rs i j in
                      elem c block

isAllowed :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed rs i j c  = isValidChar rs c &&                      
                      not (isInRow rs i c) &&
                      not (isInRow (transpose rs) j c) &&
                      not (isInBlock rs i j c)

isAllowed' :: Sudoku -> Int -> Int -> Char -> Bool
isAllowed' rs i j c = isAllowed rs i j c && not (isTaken rs i j)

allowedChars :: Sudoku -> String
allowedChars su = take (columnCount su) "123456789ABCDEFG"

isValidChar :: Sudoku -> Char -> Bool
isValidChar rs c = elem c (allowedChars rs)

update :: Sudoku -> Char -> Int -> Int -> Sudoku
update rs c i j = mapWithIndeces rs f
                where f i' j' | i' == i && j == j' = c
                              | otherwise = rs !! i' !! j'

mapWithIndeces :: Sudoku -> (Int -> Int -> b) -> [[b]]
mapWithIndeces rs f = let (r, c) = (rowCount rs - 1, columnCount rs - 1) in
                      map (\i -> (map (f i) [0..c])) [0..r]

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs

