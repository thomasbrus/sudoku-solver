module Sudoku.GUI.Button (Button (..), compose, update, inBoundary) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State

data Button = Rectangular (Float, Float, Float, Float) String (Float, Float) Bool Color

compose :: State -> Button -> Picture
compose _ (Rectangular (x, y, width, height) text (tw, th) highlighted color)
  | highlighted
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.05) $ rectangleSolid width height
  , Translate x y $ Color color $ rectangleWire width height
  , Translate tx ty $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  | otherwise
  = Pictures
  [ Translate x y $ Color (makeColor 0.8 0.8 0.8 1) $ rectangleWire width height
  , Translate tx ty $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)

update :: Float -> Float -> Button -> Button
update mx my btn
  | inBoundary mx my btn
  = Rectangular coords text td True color
  | otherwise
  = Rectangular coords text td False color
  where
    (Rectangular coords text td _ color) = btn

inBoundary :: Float -> Float -> Button -> Bool
inBoundary mx my (Rectangular coords text td _ color)
  = mx >= x1 && mx <= x2 && my >= y1 && my <= y2
  where
    (x, y, width, height) = coords
    (x1, y1) = (x - (width / 2), y - (height / 2))
    (x2, y2) = (x + (width / 2), y + (height / 2))


