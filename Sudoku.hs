module Sudoku where

import Data.Char

type Sudoku = [[Char]]

exampleSudoku =
  [ "4.....938"
  , ".32.941.."
  , ".953..24."
  , "37.6.9..4"
  , "529..1673"
  , "6.47.3.9."
  , "957..83.."
  , "..39..4.."
  , "24..3.7.9" ]

rowCount :: Sudoku -> Int
rowCount rs = length rs

columnCount :: Sudoku -> Int
columnCount rs = length $ head rs

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs