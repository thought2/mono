module Test.Debug where

import Prelude
import Board (Board)
import Board as Board
import Data.Either (Either(..))
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Console as Console

sampleBoard :: Board
sampleBoard = Board.init (vec2 3 3)

prettyLog :: Board -> Effect Unit
prettyLog board = Console.log $ Board.prettyPrint board

prettyLog' :: forall err. Show err => Either err Board -> Effect Unit
prettyLog' eitherBoard = case eitherBoard of
  Left err -> Console.log (show err)
  Right board -> Console.log $ Board.prettyPrint board

main :: Effect Unit
main = do
  prettyLog sampleBoard
