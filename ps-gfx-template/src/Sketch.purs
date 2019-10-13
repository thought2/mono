module Sketch where

import Prelude
import Graphics.Drawing
import Color

draw :: Number -> Number -> Drawing
draw width height =
  circle (width / 2.0) (height / 2.0) (width / 100.0)
    # filled (fillColor black)
