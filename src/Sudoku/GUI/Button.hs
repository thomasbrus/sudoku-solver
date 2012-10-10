module Sudoku.GUI.Button (Button (..), draw, drawAll, inBoundary) where

import Prelude
import FPPrac.Events
import FPPrac.Graphics
import Sudoku.GUI.State

data Button = Rectangular (Float, Float, Float, Float) String (Float, Float) Color

draw :: State -> Input -> Button -> Picture
draw s (MouseMotion (mx, my)) (Rectangular (x, y, width, height) txt (tw, th) clr)
  | inBoundary mx my btn && mousePressed s
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.02) $ rectangleSolid width height
  , Translate x y $ Color clr $ rectangleWire width height
  , Translate tx (ty - 2) $ Color clr $ Scale 0.2 0.2 $ Text txt
  ]
  | inBoundary mx my btn && not (mousePressed s)
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.05) $ rectangleSolid width height
  , Translate x y $ Color clr $ rectangleWire width height
  , Translate tx ty $ Color clr $ Scale 0.2 0.2 $ Text txt
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)
    btn = (Rectangular (x, y, width, height) txt (tw, th) clr)

draw s (MouseDown (mx, my)) (Rectangular (x, y, width, height) txt (tw, th) clr)
  | inBoundary mx my btn && mousePressed s
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.02) $ rectangleSolid width height
  , Translate x y $ Color clr $ rectangleWire width height
  , Translate tx (ty - 2) $ Color clr $ Scale 0.2 0.2 $ Text txt
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)
    btn = (Rectangular (x, y, width, height) txt (tw, th) clr)

draw _ _ (Rectangular (x, y, width, height) txt (tw, th) clr)
  = Pictures
  [ Translate x y $ Color (makeColor 0.8 0.8 0.8 1) $ rectangleWire width height
  , Translate tx ty $ Color clr $ Scale 0.2 0.2 $ Text txt
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)

drawAll :: State -> Input -> [Button] -> Picture
drawAll s e btns = Pictures $ map (draw s e) btns

inBoundary :: Float -> Float -> Button -> Bool
inBoundary mx my (Rectangular coords _ _ _)
  = mx >= x1 && mx <= x2 && my >= y1 && my <= y2
  where
    (x, y, width, height) = coords
    (x1, y1) = (x - 1 - (width / 2), y - 1 - (height / 2))
    (x2, y2) = (x +  1 + (width / 2), y + 1 + (height / 2))


