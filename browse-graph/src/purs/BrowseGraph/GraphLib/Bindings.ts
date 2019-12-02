import * as graphlib from "graphlib";
import * as GraphSpec from "../../../ts/GraphSpec";

type Id = string;

export type GraphSpec<Graph, Edge, Node> = GraphSpec.GraphSpec<
  Id,
  Graph,
  Edge,
  Node
>;

export const specToGraph = <G, E, N>(
  graphSpec: GraphSpec<G, E, N>
) => (): graphlib.Graph<G, E, N> => {
  const graph: graphlib.Graph<G, E, N> = new graphlib.Graph();

  graph.setGraph(graphSpec.graph.label);

  graphSpec.nodes.forEach(node => graph.setNode(node.id, node.label));

  graphSpec.edges.forEach(edge =>
    graph.setEdge(edge.fromId, edge.toId, edge.label)
  );

  return graph;
};

export const graphToSpec = <G, E, N>(
  graph: graphlib.Graph<G, E, N>
) => (): GraphSpec<G, E, N> | null => {
  const graphLabel = graph.graph();
  if (!graphLabel) return null;

  const nodes = [];
  for (const nodeId of graph.nodes()) {
    const nodeLabel = graph.node(nodeId);
    if (!nodeLabel)
      throw new Error(`Invalid Graph: Node Id ${nodeId} does not exist.`);
    nodes.push({ id: nodeId, label: nodeLabel });
  }

  const edges = [];
  for (const edgeObj of graph.edges()) {
    const edgeLabel = graph.edge(edgeObj.v, edgeObj.w);
    if (!edgeLabel) throw new Error("Invalid Graph 2");
    edges.push({ fromId: edgeObj.v, toId: edgeObj.w, label: edgeLabel });
  }

  return { graph: { label: graphLabel }, nodes, edges };
};
