module Main where

import Prelude
import BrowseGraph.Graph as BrowseGraph.Graph
import BrowseGraph.Web.Storage.Storage.Extra as BrowseGraph.Web.Storage.Storage.Extra
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.Except as ExceptT
import Control.Monad.Trans.Class (lift)
import Data.Argonaut as Data.Argonaut
import Data.Either as Either
import Data.Graph as Graph
import Effect (Effect)
import Effect.Console (log, logShow)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Web.HTML.Window
import Web.Storage.Storage as Web.Storage.Storage

type Graph'
  = Graph.Graph String { url :: String }

foreign import io :: Effect Unit

type Vec
  = { x :: Number, y :: Number }

type Graph
  = BrowseGraph.Graph.Graph
      String
      { format :: { size :: Vec }
      , data_ :: Unit
      }
      { format :: { pos :: Vec, points :: Array Vec }
      , data_ :: Unit
      }
      { format :: { pos :: Vec }
      , data_ :: Unit
      }

storageOperations ::
  Web.Storage.Storage.Storage ->
  { get :: ExceptT String Effect Graph'
  , set :: Graph' -> ExceptT String Effect Unit
  }
storageOperations storage =
  let
    key = "browse-graph"

    set graph =
      Graph.toMap graph
        # Data.Argonaut.encodeJson
        # Data.Argonaut.stringify
        # ( \value ->
              lift $ Web.Storage.Storage.setItem key value storage
          )

    get =
      ( Web.Storage.Storage.getItem key storage
          <#> Either.note "Item not available"
          # ExceptT
      )
        >>= ( \value ->
              Data.Argonaut.jsonParser value
                >>= Data.Argonaut.decodeJson
                <#> Graph.fromMap
                # except
          )
  in
    { set, get
    }

run :: ExceptT String Effect Unit
run = do
  window <- lift $ Web.HTML.window
  document <- lift $ Web.HTML.Window.document window
  sessionStorage <- lift $ Web.HTML.Window.sessionStorage window
  let
    key = "key"
  currentUrl <- lift $ Web.HTML.Window.location window >>= Web.HTML.Location.href
  previousUrl <- lift $ Web.HTML.HTMLDocument.referrer document
  graph :: Graph <-
    ( ( BrowseGraph.Web.Storage.Storage.Extra.getItem key sessionStorage
          <#> BrowseGraph.Graph.fromSpec
      )
        <|> ( pure
              $ BrowseGraph.Graph.init
                  { format: { size: { x: 0.0, y: 0.0 } }
                  , data_: unit
                  }
          )
    )
      <#> updateGraph { currentUrl, previousUrl }
  _ <-
    ( BrowseGraph.Graph.toSpec graph
        # (\graphSpec -> lift $ BrowseGraph.Web.Storage.Storage.Extra.setItem key graphSpec sessionStorage)
    )
  lift $ logShow $ BrowseGraph.Graph.toSpec graph
  pure unit
  where
  updateGraph { currentUrl, previousUrl } graph =
    graph
      # BrowseGraph.Graph.insertNode
          { id: currentUrl
          , label:
            { format: { pos: { x: 0.0, y: 0.0 } }
            , data_: unit
            }
          }
      # ( \x ->
            if previousUrl /= "" then
              BrowseGraph.Graph.insertEdge
                { fromId: previousUrl
                , toId: currentUrl
                , label:
                  { format: { pos: { x: 0.0, y: 0.0 }, points: [] }
                  , data_: unit
                  }
                }
                x
            else
              x
        )

run' :: ExceptT String Effect Unit
run' = do
  window <- lift $ Web.HTML.window
  document <- lift $ Web.HTML.Window.document window
  sessionStorage <- lift $ Web.HTML.Window.sessionStorage window
  let
    storage = storageOperations sessionStorage
  graph <- storage.get <|> pure Graph.empty
  currentUrl <- lift $ Web.HTML.Window.location window >>= Web.HTML.Location.href
  previousUrl <- lift $ Web.HTML.HTMLDocument.referrer document
  newGraph <-
    Graph.insertVertex currentUrl { url: currentUrl } graph
      # ( \graph' ->
            if previousUrl /= "" then
              Graph.insertEdge previousUrl currentUrl graph'
                # Either.note "Consistency Error"
            else
              pure graph'
        )
      # except
  storage.set $ newGraph
  lift $ logShow $ Graph.toMap newGraph
  pure unit

main :: Effect Unit
main = do
  _ <- ExceptT.runExceptT run
  log "🍝"
  io
