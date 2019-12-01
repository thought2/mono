module BrowseGraph.GraphLib.Bindings where

import Effect (Effect)
import BrowseGraph.GraphSpec as GraphSpec

foreign import data Graph :: Type -> Type -> Type -> Type

type GraphSpec g e n
  = GraphSpec.GraphSpec String g e n

foreign import specToGraph :: forall g e n. GraphSpec g e n -> Effect (Graph g e n)

foreign import graphToSpec :: forall g e n. Graph g e n -> Effect (GraphSpec g e n)
