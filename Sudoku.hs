module Sudoku where

import Data.Char

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

rowCount :: Sudoku -> Int
rowCount rs = length rs

columnCount :: Sudoku -> Int
columnCount rs = length $ head rs

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs