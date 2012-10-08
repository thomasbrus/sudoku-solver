module Sudoku.GUI.Raster (draw, handleEvents, calculateCell) where

import Prelude
import Data.Maybe
import FPPrac.Events hiding (Button)
import FPPrac.Graphics hiding (dim)
import Sudoku.GUI.State
import Sudoku (isTaken, Sudoku)

handleEvents s e = (s, [])

size = 540.0
borderColor = makeColor 1 1 1 0.5

draw :: State -> Input -> Picture
draw s e = Translate (-100) 0 $ Pictures
  -- TODO Green border if isValid
  [ Color violet $ rectangleWire (size + 2) (size + 2)
  , Translate shift shift $ Pictures tiles      
  ]
  where
    dim' = fromIntegral (dim s) :: Float
    tiles = [ drawTile s e i j | i<-[0..(dim s - 1)], j<-[0..(dim s - 1)] ]
    shift = (-size / 2) + (size / dim') / 2

drawTile :: State -> Input -> Int -> Int -> Picture
drawTile s (MouseMotion (mx, my)) i j
  | mx > x - (size / 2) - 100 &&
    mx < x - (size / 2) + cellSize - 100 &&
    my > y - (size / 2) &&
    my < y - (size / 2) + cellSize &&
    not (isTaken (sudoku s) (dim s - j - 1) i)
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

drawTile state@(State {invalidCell=sc,dim=d,sudoku=su}) _ i j
  | isJust sc && fromJust sc == (d - j - 1, i)
  = Translate (i' * cellSize) (j' * cellSize) $ Pictures
    [ Color (makeColor 1 0 0 0.2) $ rectangleSolid (cellSize - 2) (cellSize - 2)
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

showCell :: Sudoku -> Int -> Int -> String
showCell su i j | elem c "123456789" = [c]
                | otherwise = ""
                where c = su !! i !! j

calculateCell :: Float -> Float -> Int -> Maybe (Int, Int)
calculateCell mx my dim | elem i [0..(dim - 1)] && elem j [0..(dim - 1)] = Just (i, j)
                        | otherwise = Nothing
                        where
                          cellSize = size / (fromIntegral dim)
                          i = dim - ceiling ((my + (size / 2)) / cellSize)
                          j = floor ((mx + 100 + (size / 2)) / cellSize)


-- TODO rename to draw