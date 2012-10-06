module Sudoku.GUI.Raster (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State

handleEvents s e = (s, [])

compose :: State -> Float -> Picture
compose s dim = Pictures
  [ Color bgColor $ rectangleSolid (cellSize * dim) (cellSize * dim)
  , Color violet $ rectangleWire (cellSize * dim + 1) (cellSize * dim + 1)
  , Translate shift shift $ Color borderColor $ Pictures $ concat cells
  ]
  where
    f i j = Translate (i * cellSize) (j * cellSize) $ rectangleWire cellSize cellSize
    cells = map (\i -> (map (f i) [1..dim])) [1..dim]
    cellSize = 540 / dim
    bgColor = makeColor 1 1 1 0.00
    borderColor = makeColor 1 1 1 0.5
    shift = -cellSize * dim / 2 - cellSize / 2
