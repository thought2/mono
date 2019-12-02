import * as dagre from "dagre";

export const layout = <G, E, N>(
  graph: dagre.FormatGraph<G, E, N>
) => (): void => {
  dagre.layout(graph);
};
