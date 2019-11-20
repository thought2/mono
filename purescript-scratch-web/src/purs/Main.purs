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

initState :: State
initState = 0

main :: Effect Unit
main = do
  state <- HotReload.init def initState
  Console.logShow state
  _ <- HotReload.saveSnapshot def (state + 1)
  log "AAAAA"
