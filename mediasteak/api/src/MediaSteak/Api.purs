module MediaSteak.Api where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

derive instance genericErrGet :: Generic ErrGet _

instance showErrGet :: Show ErrGet where
  show = genericShow

class
  Monad m <= IO m where
  get :: String -> ExceptT ErrGet m String
