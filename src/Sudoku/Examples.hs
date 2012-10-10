module Sudoku.Examples where

import Sudoku
import Prelude

exampleSudoku :: Int -> Sudoku

exampleSudoku 4 =
  [ "2..."
  , "..1."
  , ".2.."
  , "...4"  
  ]

exampleSudoku 6 =
  [ "3....4"
  , "..43.."
  , ".3..6."
  , ".4..1."
  , "..21.."
  , "1....2"
  ]

exampleSudoku 9 =
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
