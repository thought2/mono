module Main where

import Prelude
import BrowseGraph.Dagre as BrowseGraph.Dagre
import BrowseGraph.Graph as BrowseGraph.Graph
import BrowseGraph.Web.Storage.Storage.Extra as BrowseGraph.Web.Storage.Storage.Extra
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Except as ExceptT
import Control.Monad.Trans.Class (lift)
import Data.Graph as Graph
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log, logShow)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Web.HTML.Window

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
    ( BrowseGraph.Web.Storage.Storage.Extra.getItem key sessionStorage
        <#> BrowseGraph.Graph.fromSpec
    )
      <|> ( pure
            $ BrowseGraph.Graph.init
                { format: { size: { x: 0.0, y: 0.0 } }
                , data_: unit
                }
        )
      <#> updateGraph { currentUrl, previousUrl }
      <#> (spy "A" <<< BrowseGraph.Graph.toSpec)
      <#> (spy "B" <<< BrowseGraph.Dagre.layoutBySpec)
      <#> (spy "C" <<< BrowseGraph.Graph.fromSpec)
  _ <-
    ( BrowseGraph.Graph.toSpec graph
        # (\graphSpec -> lift $ BrowseGraph.Web.Storage.Storage.Extra.setItem key graphSpec sessionStorage)
    )
  lift $ log $ unsafeCoerce $ BrowseGraph.Graph.toSpec graph
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

main :: Effect Unit
main = do
  _ <- ExceptT.runExceptT run
  log "🍝"
  io
