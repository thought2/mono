module MediaSteak.Api.StringParser where

import Prelude
import Text.Parsing.StringParser

import Data.Int as Int
import Text.Parsing.StringParser.CodeUnits (regex, string)

int :: Parser Int
int =
  regex "-?\\d+"
    <#> Int.fromString
    >>= maybe (fail "Expected an integer") pure

pageNumberFromUrl :: Parser Int
pageNumberFromUrl = do
  string "https://www.mediasteak.com/aktuell/page/"
  n <- int
  string "/"
  pure n
