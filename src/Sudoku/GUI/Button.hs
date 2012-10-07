module Sudoku.GUI.Button (Button (..), compose, composeAll, inBoundary) where

import Prelude
import FPPrac.Events
import FPPrac.Graphics
import Sudoku.GUI.State

data Button = Rectangular (Float, Float, Float, Float) String (Float, Float) Color

compose :: State -> Input -> Button -> Picture
compose s (MouseMotion (mx, my)) (Rectangular (x, y, width, height) text (tw, th) color)
  | mousePressed s
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.02) $ rectangleSolid width height
  , Translate x y $ Color color $ rectangleWire width height
  , Translate tx (ty - 2) $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  | inBoundary mx my btn && not (mousePressed s)
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.05) $ rectangleSolid width height
  , Translate x y $ Color color $ rectangleWire width height
  , Translate tx ty $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)
    btn = (Rectangular (x, y, width, height) text (tw, th) color)

compose s (MouseDown (mx, my)) (Rectangular (x, y, width, height) text (tw, th) color)
  | inBoundary mx my btn && mousePressed s
  = Pictures
  [ Translate x y $ Color (makeColor 1 1 1 0.02) $ rectangleSolid width height
  , Translate x y $ Color color $ rectangleWire width height
  , Translate tx (ty - 2) $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)
    btn = (Rectangular (x, y, width, height) text (tw, th) color)

compose s _ (Rectangular (x, y, width, height) text (tw, th) color)
  = Pictures
  [ Translate x y $ Color (makeColor 0.8 0.8 0.8 1) $ rectangleWire width height
  , Translate tx ty $ Color color $ Scale 0.2 0.2 $ Text text
  ]
  where
    tx = x - (tw / 2)
    ty = y - (th / 2)

composeAll :: State -> Input -> [Button] -> Picture
composeAll s e btns = Pictures $ map (compose s e) btns

inBoundary :: Float -> Float -> Button -> Bool
inBoundary mx my (Rectangular coords _ _ _)
  = mx >= x1 && mx <= x2 && my >= y1 && my <= y2
  where
    (x, y, width, height) = coords
    (x1, y1) = (x - (width / 2), y - (height / 2))
    (x2, y2) = (x + (width / 2), y + (height / 2))


