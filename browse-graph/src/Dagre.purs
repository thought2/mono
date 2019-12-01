module Dagre
  ( layoutBySpec
  , GraphSpec
  , GraphLabel
  , EdgeLabel
  , NodeLabel
  , Vec
  ) where

import Prelude
import GraphLib.Bindings as GraphLib.Bindings
import Dagre.Bindings as Dagre.Bindings
import Partial.Unsafe (unsafeCrashWith)
import GraphSpec as GraphSpec

type Vec
  = { x :: Number, y :: Number }

type GraphLabel a
  = { format :: { size :: Vec }
    , data_ :: a
    }

type EdgeLabel a
  = { format :: { pos :: Vec, points :: Array Vec }
    , data_ :: a
    }

type NodeLabel a
  = { format :: { pos :: Vec }
    , data_ :: a
    }

type GraphSpec g e n
  = GraphLib.Bindings.GraphSpec
      (GraphLabel g)
      (EdgeLabel e)
      (NodeLabel n)

specToBindings :: forall g e n. GraphSpec g e n -> Dagre.Bindings.GraphSpec g e n
specToBindings graphSpec =
  graphSpec
    # GraphSpec.mapGraph
        ( \{ format: { size: { x, y } }, data_ } ->
            { width: x, height: y, data_ }
        )
    # GraphSpec.mapEdges
        ( \{ format: { pos, points }, data_ } ->
            { x: pos.x, y: pos.y, points, data_ }
        )
    # GraphSpec.mapNodes
        ( \{ format: { pos }, data_ } ->
            { x: pos.x, y: pos.y, data_ }
        )

specFromBindings :: forall g e n. Dagre.Bindings.GraphSpec g e n -> GraphSpec g e n
specFromBindings graphSpec =
  graphSpec
    # GraphSpec.mapGraph
        ( \{ width, height, data_ } ->
            { format: { size: { x: width, y: height } }
            , data_
            }
        )
    # GraphSpec.mapEdges
        ( \{ x, y, points, data_ } ->
            { format: { pos: { x, y }, points }
            , data_
            }
        )
    # GraphSpec.mapNodes
        ( \{ x, y, data_ } ->
            { format: { pos: { x, y } }
            , data_
            }
        )

layoutBySpec :: forall g e n. GraphSpec g e n -> GraphSpec g e n
layoutBySpec graphSpec =
  specToBindings graphSpec
    # Dagre.Bindings.layoutBySpec
    # specFromBindings
