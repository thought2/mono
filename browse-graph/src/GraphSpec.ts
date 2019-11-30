export interface GraphSpec<Id, Graph, Edge, Node> {
  graph: { label: Graph };
  nodes: Array<{ id: Id; label: Node }>;
  edges: Array<{
    fromId: Id;
    toId: Id;
    label: Edge;
  }>;
}
