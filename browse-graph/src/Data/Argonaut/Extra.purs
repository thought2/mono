module Data.Argonaut.Extra where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as Data.Argonaut
import Data.Either (Either)

decodeJsonString :: forall a. DecodeJson a => String -> Either String a
decodeJsonString = Data.Argonaut.jsonParser >=> Data.Argonaut.decodeJson

encodeJsonString :: forall a. EncodeJson a => a -> String
encodeJsonString = Data.Argonaut.encodeJson >>> Data.Argonaut.stringify
