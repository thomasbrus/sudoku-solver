module Sudoku.Strategy.NakedPairs where

import Data.Char
import Data.List
import Sudoku
import Sudoku.Strategy

type Candidates = [(Int, Int, String)]

haveUnitInCommon :: Sudoku -> (Int, Int) -> (Int, Int) -> Bool
haveUnitInCommon rs (i, j) (i', j') = i == i' || j == j' || findBlock rs i j == findBlock rs i' j'

nakedPairs :: Sudoku -> Candidates
nakedPairs rs  = concat $ filter (\x -> length x == 2) css
    where
      f (i, j, cs)                = length cs == 2
      s (_, _, c) (_, _, c')      = compare c c'
      g (i, j, cs) (i', j', cs')  = cs == cs' && haveUnitInCommon rs (i, j) (i', j')
      css = groupBy g $ sortBy s $ filter f $ allCandidates rs

candidatePairs :: Sudoku -> Candidates
candidatePairs rs = filter (\(i, j, cs) -> length cs == 2) $ allCandidates rs

allCandidates :: Sudoku -> Candidates
allCandidates rs  = [ (i, j, findCandidates rs i j) | i<-[0..8], j<-[0..8] ]

--findApplicableNakedPairs :: Sudoku -> Int -> Int -> [Candidates]
--findApplicableNakedPairs rs i j = filter (\) collectCandidates rs

resolveCandidates :: Sudoku -> Int -> Int -> (Char, String)
resolveCandidates rs i j  = (rs !! i !! j, cs \\ es)
                          where
                            cs = findCandidates rs i j
                            es = "1"

resolveAllCandidates :: Sudoku -> [[(Char, String)]]
resolveAllCandidates rs = mapWithIndeces rs (\rs i j -> resolveCandidates rs i j)





