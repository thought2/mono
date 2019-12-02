module BrowseGraph.Graph
  ( Graph
  , init
  , insertEdge
  , insertNode
  , fromSpec
  , toSpec
  ) where

import Prelude
import BrowseGraph.GraphSpec as BrowseGraph.GraphSpec
import Data.Array as Array

newtype Graph id graph edge node
  = Graph (BrowseGraph.GraphSpec.GraphSpec id graph edge node)

fromSpec :: forall i g e n. BrowseGraph.GraphSpec.GraphSpec i g e n -> Graph i g e n
fromSpec graphSpec = Graph graphSpec

toSpec :: forall i g e n. Graph i g e n -> BrowseGraph.GraphSpec.GraphSpec i g e n
toSpec (Graph graphSpec) = graphSpec

init :: forall i g e n. g -> Graph i g e n
init graphLabel =
  Graph
    { graph: { label: graphLabel }
    , edges: []
    , nodes: []
    }

insertNode :: forall i g e n. { id :: i, label :: n } -> Graph i g e n -> Graph i g e n
insertNode nodeSpec (Graph graph) =
  Graph
    $ graph
        { nodes = Array.snoc graph.nodes nodeSpec
        }

insertEdge :: forall i g e n. { fromId :: i, toId :: i, label :: e } -> Graph i g e n -> Graph i g e n
insertEdge edgeSpec (Graph graph) =
  Graph
    $ graph
        { edges = Array.snoc graph.edges edgeSpec
        }
