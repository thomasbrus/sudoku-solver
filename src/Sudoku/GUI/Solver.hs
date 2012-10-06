module Sudoku.GUI.Solver (compose, handleEvents) where

import Prelude
import FPPrac.Events hiding (Button)
import FPPrac.Graphics
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Solver.FourTimesFour as FourTimesFour
import qualified Sudoku.GUI.Solver.NineTimesNine as NineTimesNine
import qualified Sudoku.GUI.Solver.TwelveTimesTwelve as TwelveTimesTwelve

backToMenuBtn = Btn.Rectangular (290, (-240), 190, 60) "Go Back" (108, 21) False (greyN 0.5)

handleEvents s (MouseMotion (mx, my))
  = (s, [ScreenClear, DrawOnBuffer False, DrawPicture $ Pictures
    [ compose s
    , Btn.compose s $ Btn.update mx my backToMenuBtn
    ]
  ])

-- TODO: Events.hs ->

--handleEvents s (MouseDown (mx, my)) 
--  | Btn.inBoundary mx my backToMenuBtn
--  = (s, [DrawPicture $ Menu.compose s])
--  where
--    s = State "menu" ""

handleEvents s e = (s, [])

compose :: State -> Picture
compose s = Pictures
  [ composeBackground
  , Translate (-100) 0 $ composeRaster s
  , Btn.compose s backToMenuBtn
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
