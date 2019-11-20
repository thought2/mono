module Main where

import Prelude
import Data.Default (def)
import Data.Newtype (over, wrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Console
import HotReload as HotReload

type State
  = Int

id :: HotReload.Id
id = HotReload.Id "appState"

initState :: State
initState = HotReload.init id HotReload.defaultInitConfig 0

main :: Effect Unit
main = do
  Console.logShow initState
  let
    state2 = HotReload.saveSnapshot id (initState + 1)
  log "AAAAAAAA"
