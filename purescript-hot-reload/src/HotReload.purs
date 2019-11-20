module HotReload (init, saveSnapshot, defaultInitConfig, InitConfig, module HotReload.IdRef) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe(..), maybe)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import HotReload.IdRef (Id(Id))
import HotReload.IdRef as IdRef

newtype MigrateFn
  = MigrateFn (Array Json -> Json -> Json)

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
  Id -> InitConfig -> a -> a
init id { cache, migrateFn } initState =
  unsafePerformEffect
    $ do
        res :: Maybe Cell <- IdRef.read id
        case res of
          Nothing -> do
            IdRef.write id
              { history: []
              , snapshot: \_ -> encodeJson initState
              }
            pure initState
          Just { history, snapshot } -> do
            let
              newHistory = Array.snoc history (snapshot unit)
            _ <- IdRef.modify (_ { history = newHistory }) id
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
                _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson initState }) id
                Console.log "State transition failed"
                pure initState
              Just x -> do
                _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson x }) id
                pure x

-- SAVE SNAPSHOT
saveSnapshot :: forall a. EncodeJson a => Id -> a -> a
saveSnapshot id x =
  unsafePerformEffect
    $ do
        _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson x }) id
        pure x
