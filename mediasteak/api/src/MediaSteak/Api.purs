module MediaSteak.Api where

import Prelude
import Control.Monad.Except (ExceptT)
import Partial.Unsafe (unsafeCrashWith)

type MediaItem
  = { title :: String
    , url :: String
    , description :: String
    , duration :: Minutes
    , image :: String
    }

type Minutes
  = Int

data ErrGet
  = ErrGet

class
  Monad m <= IO m where
  get :: String -> ExceptT ErrGet m String

getItems :: forall m. m (Array MediaItem)
getItems = unsafeCrashWith "getItems"
