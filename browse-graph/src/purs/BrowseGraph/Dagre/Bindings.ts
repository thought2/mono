import * as dagre from "dagre";
import * as GraphLib_Bindings from "../GraphLib/Bindings";

export const layoutBySpec = <G, N, E>(
  graphSpec: GraphLib_Bindings.GraphSpec<G, N, E>
): GraphLib_Bindings.GraphSpec<G, N, E> => {
  const graph = GraphLib_Bindings.specToGraph(graphSpec)();
  dagre.layout(graph);
  const newGraphSpec = GraphLib_Bindings.graphToSpec(graph)();
  if (!newGraphSpec) throw new Error("Invalid Graph");
  return newGraphSpec;
};
