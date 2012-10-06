module Sudoku.GUI.Button (Button (..), compose) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics

data Button = Rectangular (Float, Float, Float, Float) String (Float, Float) Bool Color

compose :: Button -> Picture
compose (Rectangular (x, y, width, height) t (tw, th) False color) = Pictures
  [ Translate x y $ Color (makeColor 0.8 0.8 0.8 1) $ rectangleWire width height
  , Translate tx ty $ Color color $ Scale 0.2 0.2 $ Text t
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)


