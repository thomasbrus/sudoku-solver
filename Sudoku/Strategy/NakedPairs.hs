module Sudoku.Strategy.NakedPairs where

import Data.Char
import Data.List
import Sudoku
import Sudoku.Strategy

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

haveUnitInCommon :: Sudoku -> (Int, Int) -> (Int, Int) -> Bool
haveUnitInCommon rs (i, j) (i', j') = i == i' || j == j' || findBlock rs i j == findBlock rs i' j'

--findNakedPairs :: Sudoku -> [[(Int, Int, String)]]
--findNakedPairs rs = let css = filterCandidates rs ((==) 2) in
--                    groupBy (\(i, j, cs) (i', j', cs') -> cs == cs') css

collectNakedPairs :: Sudoku -> [[(Int, Int, String)]]
collectNakedPairs rs  = css''
                      where
                        f (i,j,cs)              = length cs == 2
                        s (_,_,c) (_,_,c')      = compare c c'
                        g (i,j,cs) (i',j',cs')  = cs == cs' && haveUnitInCommon rs (i, j) (i', j')
                        css                     = filter f $ collectCandidates rs
                        css'                    = sortBy s css
                        css''                   = groupBy g css'

collectCandidates :: Sudoku -> [(Int, Int, String)]
collectCandidates rs  = [ (i, j, findCandidates rs i j) | i<-[0..8], j<-[0..8] ]
