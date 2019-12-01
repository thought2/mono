module BrowseGraph.Dagre.Bindings where

import BrowseGraph.GraphSpec as BrowseGraph.GraphSpec
import Partial.Unsafe (unsafeCrashWith)

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

foreign import layoutBySpec :: forall g e n. GraphSpec g e n -> GraphSpec g e n
