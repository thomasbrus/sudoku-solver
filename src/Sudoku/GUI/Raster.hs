module Sudoku.GUI.Raster (draw, handleEvents, calculateCell) where

import Prelude
import Data.Maybe
import FPPrac.Events hiding (Button)
import FPPrac.Graphics hiding (dim)
import Sudoku.GUI.State
import Sudoku

handleEvents s e = (s, [])

size = 540.0
borderColor = makeColor 1 1 1 0.5
dividerColor = violet
borderWidth = 2

-- TODO indicate blocks by a thick line
-- TODO print allow values (in italics)
-- TODO green border if isValid
-- TODO let all cells be changeable

draw :: State -> Input -> Picture
draw s e = Translate (-100) 0 $ Pictures
  [ drawVerticalDividers s e
  , drawHorizontalDividers s e
  , drawFrame s e
  , Translate shift shift $ drawCells s e
  ]
  where
    dim' = fromIntegral (dim s) :: Float
    shift = (-size / 2) + (size / dim') / 2

drawCells :: State -> Input -> Picture
drawCells s e = Pictures [ drawCell s e i j | i<-[0..(dim s - 1)], j<-[0..(dim s - 1)] ]

drawCell :: State -> Input -> Int -> Int -> Picture
drawCell s (MouseMotion (mx, my)) i j
  | mx > x - (size / 2) - 100 &&
    mx < x - (size / 2) + cellSize - 100 &&
    my > y - (size / 2) &&
    my < y - (size / 2) + cellSize
    -- not (isTaken (sudoku s) (dim s - j - 1) i)
  = Translate x y $ Pictures
    [ Color violet $ rectangleWire (cellSize + 2) (cellSize + 2)
    , Color borderColor $ rectangleWire cellSize cellSize
    , Translate (-35 * scale) (-50 * scale) $ Color white $ Scale scale scale $ Text "..."
    ]
  where
    [i', j', dim']  = map fromIntegral [i, j, dim s]
    cellSize        = (size / dim')
    x               = (i' * cellSize)
    y               = (j' * cellSize)
    scale           = 2.5 / dim'

drawCell state@(State {invalidCell=sc,dim=d,sudoku=su}) _ i j
  | isJust sc && fromJust sc == (d - j - 1, i)
  = Translate (i' * cellSize) (j' * cellSize) $ Pictures
    [ Color (dark (makeColor 1 0 0 0.5)) $ rectangleSolid (cellSize - 2) (cellSize - 2)
    , Translate (-35 * scale) (-50 * scale) $ Color white $ Scale scale scale $ Text "X"
    ]
  | otherwise
  = Translate (i' * cellSize) (j' * cellSize) $ Pictures
    [ Color borderColor $ rectangleWire cellSize cellSize
    , Translate (-35 * scale) (-50 * scale) $ Color (greyN 0.75) $ Scale scale scale $ Text text
    ]
  where
    [i', j', dim']  = map fromIntegral [i, j, d]
    cellSize        = (size / dim')
    text            = showCell su (d - j - 1) i
    scale           = 2 / dim'


drawFrame :: State -> Input -> Picture
drawFrame s i = Color violet $ rectangleWire (size + borderWidth) (size + borderWidth)

drawVerticalDividers :: State -> Input -> Picture
drawVerticalDividers s e = Pictures
  [ drawVerticalDivider s e i | i<-(filter (\x -> mod x bw == 0) [1..(dim s)]) ]
  where bw = blockWidth (sudoku s)
  
drawVerticalDivider :: State -> Input -> Int -> Picture
drawVerticalDivider s e i = let cellSize = (size / fromIntegral (dim s)) in
  Translate ((-size / 2) + (cellSize * fromIntegral i)) 0 $ Color dividerColor $ rectangleSolid borderWidth size

drawHorizontalDividers :: State -> Input -> Picture
drawHorizontalDividers s e = Pictures
  [ drawHorizontalDivider s e i | i<-(filter (\x -> mod x bh == 0) [1..(dim s)]) ]
  where bh = blockHeight (sudoku s)

drawHorizontalDivider :: State -> Input -> Int -> Picture
drawHorizontalDivider s e i = let cellSize = (size / fromIntegral (dim s)) in
  Translate 0 ((-size / 2) + (cellSize * fromIntegral i)) $ Color dividerColor $ rectangleSolid size borderWidth

showCell :: Sudoku -> Int -> Int -> String
showCell su i j | isValidChar su c  = [c]
                | otherwise         = ""
                where c = su !! i !! j

calculateCell :: Float -> Float -> Int -> Maybe (Int, Int)
calculateCell mx my dim | elem i [0..(dim - 1)] && elem j [0..(dim - 1)] = Just (i, j)
                        | otherwise = Nothing
                        where
                          cellSize = size / (fromIntegral dim)
                          i = dim - ceiling ((my + (size / 2)) / cellSize)
                          j = floor ((mx + 100 + (size / 2)) / cellSize)


