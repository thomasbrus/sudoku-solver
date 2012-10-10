module Sudoku.Examples where

import Sudoku
import Prelude
import Data.List (replicate)

example4x4Sudoku =
  [ "2..."
  , "..1."
  , ".2.."
  , "...4"  
  ]

example6x6Sudoku =
  [ "3....4"
  , "..43.."
  , ".3..6."
  , ".4..1."
  , "..21.."
  , "1....2"
  ]

example9x9Sudoku =
  [ "4...53..."
  , "...294.73"
  , "3...7.5.9"
  , "78.....1."
  , "........."
  , ".4.....25"
  , "2.8.1...7"
  , "91.762..."
  , "...83...2" ]

emptySudoku :: Int -> Sudoku
emptySudoku d = replicate d (replicate d '.')
