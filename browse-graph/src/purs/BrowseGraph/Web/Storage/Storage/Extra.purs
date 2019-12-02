module BrowseGraph.Web.Storage.Storage.Extra where

import Prelude
import BrowseGraph.Data.Argonaut.Extra as BrowseGraph.Data.Argonaut.Extra
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as ExceptT
import Data.Argonaut as Data.Argonaut
import Data.Either as Either
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Web.Storage.Storage as Web.Storage.Storage

getItem ::
  forall a.
  Data.Argonaut.DecodeJson a =>
  String -> Web.Storage.Storage.Storage -> ExceptT String Effect a
getItem key storage =
  Web.Storage.Storage.getItem key storage
    <#> Either.note ("'" <> key <> "' not found")
    # ExceptT
    >>= (ExceptT.except <<< BrowseGraph.Data.Argonaut.Extra.decodeJsonString)

setItem ::
  forall a.
  Data.Argonaut.EncodeJson a =>
  String -> a -> Web.Storage.Storage.Storage -> Effect Unit
setItem key value storage =
  BrowseGraph.Data.Argonaut.Extra.encodeJsonString value
    # (\valueStr -> Web.Storage.Storage.setItem key valueStr storage)
