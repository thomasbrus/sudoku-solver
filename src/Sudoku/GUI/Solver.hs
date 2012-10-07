module Sudoku.GUI.Solver (compose, buttons) where

import Prelude
import FPPrac.Events (Input)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Solver.FourTimesFour as FourTimesFour
import qualified Sudoku.GUI.Solver.NineTimesNine as NineTimesNine
import qualified Sudoku.GUI.Solver.TwelveTimesTwelve as TwelveTimesTwelve

buttons =
  [ Btn.Rectangular (290, (-240), 190, 60) "Go Back" (108, 21) (greyN 0.5)
  ]

compose :: State -> Input -> Picture
compose s e = Pictures
  [ composeBackground, Translate (-100) 0 $ composeRaster s, Btn.composeAll s e buttons
  ]

composeRaster :: State -> Picture
composeRaster s
  | v == "4 x 4"   = FourTimesFour.compose s
  | v == "9 x 9"   = NineTimesNine.compose s
  | v == "12 x 12" = TwelveTimesTwelve.compose s
  where v = variant s

composeBackground :: Picture
composeBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  ]

