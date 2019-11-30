import * as graphlib from "graphlib";

type Id = string;

export interface GraphSpec<G, E, N> {
  graph: { label: G };
  nodes: Array<{ id: Id; label: N }>;
  edges: Array<{
    fromId: Id;
    toId: Id;
    label: E;
  }>;
}

export const specToGraph = <G, E, N>(
  graphSpec: GraphSpec<G, E, N>
): graphlib.Graph<G, E, N> => {
  const graph: graphlib.Graph<G, E, N> = new graphlib.Graph();

  graph.setGraph(graphSpec.graph.label);

  graphSpec.nodes.forEach(node => graph.setNode(node.id, node.label));

  graphSpec.edges.forEach(edge =>
    graph.setEdge(edge.fromId, edge.toId, edge.label)
  );

  return graph;
};

export const graph2spec = <G, E, N>(
  graph: graphlib.Graph<G, E, N>
): GraphSpec<G, E, N> | null => {
  const graphLabel = graph.graph();
  if (!graphLabel) return null;

  const nodes = [];
  for (const nodeId of graph.nodes()) {
    const nodeLabel = graph.node(nodeId);
    if (!nodeLabel) throw new Error("Invalid Graph");
    nodes.push({ id: nodeId, label: nodeLabel });
  }

  const edges = [];
  for (const edgeObj of graph.edges()) {
    const edgeLabel = graph.edge(edgeObj.v, edgeObj.w);
    if (!edgeLabel) throw new Error("Invalid Graph");
    edges.push({ fromId: edgeObj.v, toId: edgeObj.w, label: edgeLabel });
  }

  return { graph: { label: graphLabel }, nodes, edges };
};
