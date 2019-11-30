import * as dagre from "dagre";
import * as GraphLib from "./GraphLib";

export const layout_ = <G, N, E>(
  graphSpec: GraphLib.GraphSpec<G, N, E>
): GraphLib.GraphSpec<G, N, E> => {
  const graph = GraphLib.specToGraph(graphSpec);
  dagre.layout(graph);
  const newGraphSpec = GraphLib.graph2spec(graph);
  if (!newGraphSpec) throw new Error("Invalid Graph");
  return newGraphSpec;
};
