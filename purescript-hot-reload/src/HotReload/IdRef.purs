module HotReload.IdRef where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)

newtype Id
  = Id String

foreign import write :: forall a. Id -> a -> Effect Unit

foreign import _read :: forall a. Id -> Effect (Nullable a)

read :: forall a. Id -> Effect (Maybe a)
read x = map Nullable.toMaybe $ _read x

modify :: forall a. (a -> a) -> Id -> Effect (Maybe a)
modify f id = do
  maybeVal <- read id
  case maybeVal of
    Nothing -> pure Nothing
    Just val -> do
      let
        newVal = f val
      _ <- write id newVal
      pure $ Just newVal
