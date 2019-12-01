declare module 'graphlib' {
  type Id = string;

  interface EdgeObj {
    v: Id;
    w: Id;
  }

  class Graph<G, E, N> {
    constructor(opt?: {
      directed?: boolean;
      multigraph?: boolean;
      compound?: boolean;
    });

    setGraph<G2>(label: G): Graph<G, E, N>;

    graph(): G | undefined;

    setNode(id: Id, label: N): Graph<G, E, N>;

    nodes(): Array<Id>;

    setEdge(fromId: Id, toId: Id, label: E): Graph<G, E, N>;

    node(id: Id): N | undefined;

    edges(): Array<EdgeObj>;

    edge(v: Id, w: Id): E | undefined;
  }
}
