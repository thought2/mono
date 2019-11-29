module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

foreign import io :: Effect Unit

main :: Effect Unit
main = do
  log "🍝"
  io
