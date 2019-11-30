module GraphSpec where

import Prelude

type GraphSpec id graph edge node
  = { graph :: { label :: graph }
    , nodes :: Array { id :: id, label :: node }
    , edges :: Array { fromId :: id, toId :: id, label :: edge }
    }

mapGraph ::
  forall i g1 g2 e n.
  (g1 -> g2) -> GraphSpec i g1 e n -> GraphSpec i g2 e n
mapGraph f graphSpec = graphSpec { graph = { label: f graphSpec.graph.label } }

mapNodes ::
  forall i g e n1 n2.
  (n1 -> n2) -> GraphSpec i g e n1 -> GraphSpec i g e n2
mapNodes f graphSpec = graphSpec { nodes = map mapNode graphSpec.nodes }
  where
  mapNode node = node { label = f node.label }

mapEdges ::
  forall i g e1 e2 n.
  (e1 -> e2) -> GraphSpec i g e1 n -> GraphSpec i g e2 n
mapEdges f graphSpec = graphSpec { edges = map mapEdge graphSpec.edges }
  where
  mapEdge edge = edge { label = f edge.label }
