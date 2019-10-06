module Main where

import Prelude
import Effect (Effect)
import Test.Debug as TestDebug

main :: Effect Unit
main = do
  TestDebug.main
