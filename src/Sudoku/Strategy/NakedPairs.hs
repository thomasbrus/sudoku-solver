module Sudoku.Strategy.NakedPairs where

import Data.Char
import Data.List
import Prelude
import Sudoku
import Sudoku.Strategy

haveUnitInCommon :: Sudoku -> (Int, Int) -> (Int, Int) -> Bool
haveUnitInCommon rs (i, j) (i', j') = i == i' || j == j' || findBlock rs i j == findBlock rs i' j'

nakedPairs :: Sudoku -> [(String, (Int, Int), (Int, Int))]
nakedPairs rs = map (\[(a, b, cs), (c, d, _)] -> (cs, (a, b), (c, d))) (filter (\x -> length x == 2) css)
    where
      f (i, j, cs)                = length cs == 2
      s (_, _, c) (_, _, c')      = compare c c'
      g (i, j, cs) (i', j', cs')  = cs == cs' && haveUnitInCommon rs (i, j) (i', j')
      css = groupBy g $ sortBy s $ filter f $ allCandidates rs

findExcludableCandidates :: Sudoku -> Int -> Int -> [Char]
findExcludableCandidates rs i j = concat (map (\(cs, _, _) -> cs) es)
                                  where
                                    es = filter (\(_, ab, cd) -> haveUnitInCommon rs (i, j) ab &&  haveUnitInCommon rs (i, j) cd) (nakedPairs rs)

candidatePairs :: Sudoku -> [(Int, Int, String)]
candidatePairs rs = filter (\(i, j, cs) -> length cs == 2) $ allCandidates rs

allCandidates :: Sudoku -> [(Int, Int, String)]
allCandidates rs  = [ (i, j, findCandidates rs i j) | i<-[0..r], j<-[0..c] ]
                  where
                    r = rowCount rs - 1
                    c = columnCount rs - 1

resolveCandidates :: Sudoku -> Int -> Int -> (Char, String)
resolveCandidates rs i j  | cs /= es = (s, cs \\ es)
                          | otherwise = (rs !! i !! j, cs)
                          where
                            s   = rs !! i !! j
                            cs  = findCandidates rs i j
                            es  = findExcludableCandidates rs i j

resolveAllCandidates :: Sudoku -> [[(Char, String)]]
resolveAllCandidates rs = mapWithIndeces rs (\rs i j -> resolveCandidates rs i j)

solve :: Sudoku -> Sudoku
solve rs = run rs resolveAllCandidates

