module BrowseGraph.Dagre.Bindings where

import Prelude
import BrowseGraph.GraphLib.Bindings as BrowseGraph.GraphLib.Bindings
import BrowseGraph.GraphSpec as BrowseGraph.GraphSpec
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

type NodeLabel_ a
  = { x :: Number, y :: Number, data_ :: a }

type GraphLabel_ a
  = { width :: Number, height :: Number, data_ :: a }

type EdgeLabel_ a
  = { x :: Number
    , y :: Number
    , points :: Array { x :: Number, y :: Number }
    , data_ :: a
    }

type GraphSpec g e n
  = BrowseGraph.GraphSpec.GraphSpec
      String
      (GraphLabel_ g)
      (EdgeLabel_ e)
      (NodeLabel_ n)

foreign import layout :: forall g e n. BrowseGraph.GraphLib.Bindings.Graph g e n -> Effect Unit

layoutBySpec :: forall g e n. GraphSpec g e n -> GraphSpec g e n
layoutBySpec graphSpec =
  unsafePerformEffect
    $ do
        graph <- BrowseGraph.GraphLib.Bindings.specToGraph graphSpec
        _ <- layout graph
        BrowseGraph.GraphLib.Bindings.graphToSpec graph
