module Sudoku.GUI.Raster (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics hiding (dim)
import Sudoku.GUI.State

handleEvents s e = (s, [])

compose :: State -> Input -> Picture
compose s e = Translate (-100) 0 $ Pictures
  [ Color bgColor $ rectangleSolid (cellSize * dim') (cellSize * dim')
  , Color violet $ rectangleWire (cellSize * dim' + 1) (cellSize * dim' + 1)
  , Translate shift shift $ Color borderColor $ Pictures $ concat cells
  ]
  where
    dim' = fromIntegral (dim s)
    cells = map (\i -> (map (f i) [1..dim'])) [1..dim']
    cellSize = 540 / dim'
    bgColor = makeColor 1 1 1 0.00
    borderColor = makeColor 1 1 1 0.5
    shift = -cellSize * dim' / 2 - cellSize / 2
    f i j = Translate (i * cellSize) (j * cellSize) $ Pictures
      [ rectangleWire cellSize cellSize
      , Pictures tileOnHover
      ]
    tileOnHover
      | True
      = [rectangleSolid (cellSize - 4) (cellSize - 4)]
      | otherwise
      = []


