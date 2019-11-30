module Dagre (layout) where

import GraphLib as GraphLib
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

type GraphSpec_ g e n
  = GraphLib.GraphSpec_
      (GraphLabel_ g)
      (EdgeLabel_ e)
      (NodeLabel_ n)

foreign import layout_ :: forall g e n. GraphSpec g e n -> GraphSpec g e n

type GraphFormat
  = { size :: { x :: Number, y :: Number } }

type EdgeFormat
  = { pos :: { x :: Number, y :: Number }
    , points :: Array { x :: Number, y :: Number }
    }

type NodeFormat
  = { pos :: { x :: Number, y :: Number } }

type GraphSpec g e n
  = GraphLib.GraphSpec_
      ({ format :: GraphFormat, data_ :: g })
      ({ format :: EdgeFormat, data_ :: e })
      ({ format :: NodeFormat, data_ :: n })

layout :: forall g e n. GraphSpec g e n -> GraphSpec g e n
layout graphSpec = unsafeCrashWith ""
