module Dagre (layout) where

import Data.Typelevel.Num (D2)
import Data.Vec (Vec)
import GraphLib as GraphLib
import Partial.Unsafe (unsafeCrashWith)

type NodeLabel_ r
  = { x :: Number, y :: Number | r }

type GraphLabel_ r
  = { width :: Number, height :: Number | r }

type EdgeLabel_ r
  = { | r }

type GraphSpec_ g e n
  = GraphLib.GraphSpec_
      (GraphLabel_ g)
      (EdgeLabel_ e)
      (NodeLabel_ n)

foreign import layout_ :: forall g e n. GraphSpec g e n -> GraphSpec g e n

type GraphFormat
  = { size :: Vec D2 Number }

type EdgeFormat
  = {}

type NodeFormat
  = { pos :: Vec D2 Number }

type GraphSpec g e n
  = GraphLib.GraphSpec_
      ({ format :: GraphFormat, data_ :: g })
      ({ format :: EdgeFormat, data_ :: e })
      ({ format :: NodeFormat, data_ :: n })

layout :: forall g e n. GraphSpec g e n -> GraphSpec g e n
layout = unsafeCrashWith "layout"
