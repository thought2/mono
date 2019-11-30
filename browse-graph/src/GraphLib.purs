module GraphLib where

import Prelude

type Id
  = String

type GraphSpec_ g e n
  = { graph :: { label :: g }
    , nodes :: Array { id :: Id, label :: n }
    , edges :: Array { fromId :: Id, toId :: Id, label :: e }
    }

overGraph :: forall g1 g2 e n. (g1 -> g2) -> GraphSpec_ g1 e n -> GraphSpec_ g2 e n
overGraph f graphSpec = graphSpec { graph = { label: f graphSpec.graph.label } }

overNodes :: forall g e n1 n2. (n1 -> n2) -> GraphSpec_ g e n1 -> GraphSpec_ g e n2
overNodes f graphSpec = graphSpec { nodes = map mapNode graphSpec.nodes }
  where
  mapNode node = node { label = f node.label }

overEdges :: forall g e1 e2 n. (e1 -> e2) -> GraphSpec_ g e1 n -> GraphSpec_ g e2 n
overEdges f graphSpec = graphSpec { edges = map mapEdge graphSpec.edges }
  where
  mapEdge edge = edge { label = f edge.label }
