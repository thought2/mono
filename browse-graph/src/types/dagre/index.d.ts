declare module 'dagre' {
  type FormatGraph<G, E, N> = import('graphlib').Graph<G, E, N & Node>;

  interface Node extends Size, Pos {}

  interface Size {
    width?: number;
    height?: number;
  }

  interface Pos {
    x?: number;
    y?: number;
  }

  function layout<G, E, N>(graph: FormatGraph<G, E, N>): void;
}
