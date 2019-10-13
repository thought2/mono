module Main where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafePartial)
import Sketch as Sketch
import Web.HTML.Window as Window
import Web.HTML as HTML
import Data.Int as Int

main :: Effect Unit
main = do
  window <- HTML.window
  maybeCanvas <- Canvas.getCanvasElementById "canvas"
  let
    canvas = unsafePartial (fromJust maybeCanvas)
  ctx <- Canvas.getContext2D canvas
  width <- Window.innerWidth window <#> Int.toNumber
  Canvas.setCanvasWidth canvas width
  height <- Window.innerHeight window <#> Int.toNumber
  Canvas.setCanvasHeight canvas height
  Drawing.render ctx $ Sketch.draw width height
