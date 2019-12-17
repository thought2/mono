import Vector2d from "./Vector2d";
import Grid2d from "./Grid2d";

export default class MagicSquare {
  grid: Grid2d<number>;
  constructor(size: Vector2d) {
    const grid = new Grid2d(size, 0);

    let position = new Vector2d((size.x - 1) / 2, 0);

    for (let i = 1; i <= size.x * size.y; i++) {
      grid.setCell(position, i);

      const rightUp = position
        .copy()
        .add(new Vector2d(1, -1))
        .mod(size);

      if (grid.getCell(rightUp) === 0) {
        position = rightUp;
      } else {
        position.add(new Vector2d(0, 1));
      }
    }

    this.grid = grid;
  }
}
