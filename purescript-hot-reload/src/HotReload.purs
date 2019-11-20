module HotReload where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Default (class Default, def)
import Data.Either as Either
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console as Console
import HotReload.IdRef as IdRef
import Partial.Unsafe (unsafeCrashWith)

newtype MigrateFn
  = MigrateFn (Array Json -> Json -> Json)

newtype Id
  = Id IdRef.Id

-- INIT
type Cell
  = { history :: Array Json
    , snapshot :: Unit -> Json
    }

init ::
  forall a.
  EncodeJson a =>
  DecodeJson a =>
  { id :: Id, migrateState :: MigrateFn } -> a -> Effect a
init { id: Id id, migrateState: MigrateFn migrateFn } initState = do
  Console.log "xxx"
  res :: Maybe Cell <- IdRef.read id
  case res of
    Nothing -> do
      IdRef.write id
        { history: []
        , snapshot: \_ -> encodeJson initState
        }
      Console.log "a"
      pure initState
    Just { history, snapshot } -> do
      let
        newHistory = Array.snoc history (snapshot unit)
      _ <- IdRef.modify (_ { history = newHistory }) id
      let
        newSnapshot = migrateFn newHistory (encodeJson initState)

        newX :: Maybe a
        newX = Either.hush $ decodeJson newSnapshot
      Console.log "b"
      case newX of
        Nothing -> do
          _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson initState }) id
          Console.log "State transition failed"
          pure initState
        Just x -> do
          _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson x }) id
          pure x

{-
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
        newSnapshot = migrateFn newHistory (encodeJson initState)

        newX = Either.hush $ decodeJson newSnapshot
      case newX of
        Nothing -> do
          _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson initState }) id
          Console.log "State transition failed"
          pure initState
        Just x -> do
          _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson x }) id
          pure x
-}
-- SAVE SNAPSHOT
saveSnapshot :: forall a. EncodeJson a => { id :: Id } -> a -> Effect Unit
saveSnapshot { id: Id id } x = do
  _ <- IdRef.modify (_ { snapshot = \_ -> encodeJson x }) id
  pure unit

-- INSTANCES
instance defaultMigrateFn :: Default MigrateFn where
  def = MigrateFn $ \prev curr -> maybe curr identity (Array.last prev)

instance defaultId :: Default Id where
  def = Id (IdRef.Id "appState")

derive instance newtypeMigrateFn :: Newtype MigrateFn _

derive instance newtypeId :: Newtype Id _
