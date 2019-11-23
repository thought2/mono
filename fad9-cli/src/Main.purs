module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

foreign import foo :: Effect Unit

main :: Effect Unit
main = do
  log "hi"
  foo
  log "🍝"
