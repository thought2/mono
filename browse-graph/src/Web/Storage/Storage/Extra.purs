module Web.Storage.Storage.Extra where

import Prelude
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

alterItem :: (Maybe String -> Maybe String) -> String -> Storage -> Effect Unit
alterItem f key storage = do
  getItem key storage
    <#> f
    >>= maybe
        (removeItem key storage)
        (\value -> setItem key value storage)
