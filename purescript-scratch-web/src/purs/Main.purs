module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafeCrashWith)
import Ui.Button as Button
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    maybeApp <- HA.selectElement (QuerySelector "#app")
    case maybeApp of
      Just app -> runUI Button.component unit app
      Nothing -> unsafeCrashWith ""
