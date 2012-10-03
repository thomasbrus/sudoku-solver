import Data.Char

type Sudoku = [[Char]]

exampleSudoku =
  [ "1.......6"
  , "..6.2.7.."
  , "78945.123"
  , "...8.7..4"
  , "....3...."
  , ".9...42.1"
  , "31297..4."
  , ".4..12.78"
  , "9.8......" ]

mapEmptyCells :: Sudoku -> [Int]
mapEmptyCells rs  = map (\r -> length $ filter (not . isDigit) r) rs