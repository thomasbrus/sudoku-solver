module Sudoku.Strategy.NakedPairs where

import Data.Char
import Data.List
import Prelude
import Sudoku
import Sudoku.Strategy

haveUnitInCommon :: Sudoku -> (Int, Int) -> (Int, Int) -> Bool
haveUnitInCommon su (i, j) (i', j') = i == i' || j == j' || findBlock su i j == findBlock su i' j'

nakedPairs :: Sudoku -> [(String, (Int, Int), (Int, Int))]
nakedPairs su = map (\[(a, b, cs), (c, d, _)] -> (cs, (a, b), (c, d))) (filter (\x -> length x == 2) css)
    where
      f (_, _, cs)                = length cs == 2
      s (_, _, c) (_, _, c')      = compare c c'
      g (i, j, cs) (i', j', cs')  = cs == cs' && haveUnitInCommon su (i, j) (i', j')
      css = groupBy g $ sortBy s $ filter f $ allCandidates su

findExcludableCandidates :: Sudoku -> Int -> Int -> [Char]
findExcludableCandidates su i j = concat (map (\(cs, _, _) -> cs) es)
                                  where
                                    es = filter (\(_, ab, cd) -> haveUnitInCommon su (i, j) ab &&  haveUnitInCommon su (i, j) cd) (nakedPairs su)

candidatePairs :: Sudoku -> [(Int, Int, String)]
candidatePairs su = filter (\(_, _, cs) -> length cs == 2) $ allCandidates su

allCandidates :: Sudoku -> [(Int, Int, String)]
allCandidates su  = [ (i, j, findCandidates su i j) | i<-[0..r], j<-[0..c] ]
                  where
                    r = rowCount su - 1
                    c = columnCount su - 1

resolveCandidates :: Sudoku -> Int -> Int -> (Char, String)
resolveCandidates su i j  | cs /= es = (s, cs \\ es)
                          | otherwise = (su !! i !! j, cs)
                          where
                            s   = su !! i !! j
                            cs  = findCandidates su i j
                            es  = findExcludableCandidates su i j

resolveAllCandidates :: Sudoku -> [[(Char, String)]]
resolveAllCandidates su = mapWithIndeces su (\i j -> resolveCandidates su i j)

