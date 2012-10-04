module Sudoku where

import Data.Char

type Sudoku = [[Char]]

output :: Sudoku -> IO ()
output rs = mapM_ putStrLn rs