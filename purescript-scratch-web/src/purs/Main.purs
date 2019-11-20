module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import HotReload as HotReload

type State
  = Int

id :: HotReload.Id
id = HotReload.Id "appState"

initState :: State
initState = unsafePerformEffect $ HotReload.init id HotReload.defaultInitConfig 0

main :: Effect Unit
main = do
  Console.logShow initState
  let
    state2 = unsafePerformEffect $ HotReload.saveSnapshot id (initState + 1)
  log "AA"
