module HotReload
  ( init
  , snapshot
  , unsafeInit
  , unsafeSnapshot
  , defaultInitConfig
  , InitConfig
  , Id(Id)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)

-- HOT RELOAD
type InitConfig
  = { cache :: Int
    , migrateFn :: { previous :: Array Json, current :: Json } -> Json
    }

defaultInitConfig :: InitConfig
defaultInitConfig =
  { cache: 10
  , migrateFn:
    \{ previous, current } ->
      maybe current identity (Array.last previous)
  }

type Cell
  = { history :: Array Json
    , snapshot :: Unit -> Json
    }

-- INIT
init ::
  forall a.
  EncodeJson a =>
  DecodeJson a =>
  Id -> InitConfig -> a -> Effect a
init id { cache, migrateFn } initState = do
  Console.log "init"
  res <- readCell id
  case res of
    Nothing -> do
      writeCell id
        { history: []
        , snapshot: \_ -> encodeJson initState
        }
      pure initState
    Just { history, snapshot } -> do
      let
        newHistory = Array.snoc history (snapshot unit)
      _ <- modifyCell (_ { history = newHistory }) id
      let
        newSnapshot =
          migrateFn
            { previous: newHistory
            , current: encodeJson initState
            }

        newX :: Maybe a
        newX = Either.hush $ decodeJson newSnapshot
      case newX of
        Nothing -> do
          _ <- modifyCell (_ { snapshot = \_ -> encodeJson initState }) id
          Console.log "State transition failed"
          pure initState
        Just x -> do
          _ <- modifyCell (_ { snapshot = \_ -> encodeJson x }) id
          pure x

-- SAVE SNAPSHOT
snapshot :: forall a. EncodeJson a => DecodeJson a => Id -> a -> Effect a
snapshot id x = do
  Console.log $ "snap" <> show ((\(Id i) -> i) id)
  _ <- modifyCell (_ { snapshot = \_ -> encodeJson x }) id
  pure x

-- ID REF
newtype Id
  = Id String

foreign import writeCell :: Id -> Cell -> Effect Unit

foreign import _readCell :: Id -> Effect (Nullable Cell)

readCell :: Id -> Effect (Maybe Cell)
readCell x = map Nullable.toMaybe $ _readCell x

modifyCell :: (Cell -> Cell) -> Id -> Effect (Maybe Cell)
modifyCell f id = do
  maybeVal <- readCell id
  case maybeVal of
    Nothing -> pure Nothing
    Just val -> do
      let
        newVal = f val
      _ <- writeCell id newVal
      pure $ Just newVal

-- UNSAFE
unsafeInit ::
  forall a.
  EncodeJson a =>
  DecodeJson a =>
  Id -> InitConfig -> a -> a
unsafeInit x1 x2 x3 = unsafePerformEffect $ init x1 x2 x3

unsafeSnapshot :: forall a. EncodeJson a => DecodeJson a => Id -> a -> a
unsafeSnapshot x1 x2 = unsafePerformEffect $ snapshot x1 x2
