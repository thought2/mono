module Test.Main where

import Prelude
import Effect (Effect)
import Test.Board as Board
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    Board.all
