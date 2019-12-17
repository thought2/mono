import Vector2d from "./Vector2d";
import Char from "./Char";

// 2d grid
export default class Grid2d<A extends { toString: () => string }> {
  private columns: Array<Array<A>>;
  size: Vector2d;

  constructor(size: Vector2d, fillWith: A) {
    this.columns = [];
    this.size = size;

    // Initializes all cells with a given value
    for (let x = 0; x < size.x; x++) {
      const cells = [];
      for (let y = 0; y < size.y; y++) {
        cells[y] = fillWith;
      }
      this.columns[x] = cells;
    }
  }

  setCell = (position: Vector2d, value: A): Grid2d<A> => {
    this.columns[position.x][position.y] = value;
    return this;
  };

  getCell = (position: Vector2d): A => {
    return this.columns[position.x][position.y];
  };

  // Utility methods

  getPositions(): Array<Vector2d> {
    const cells = [];
    for (let x = 0; x < this.size.x; x++) {
      for (let y = 0; y < this.size.y; y++) {
        cells.push(new Vector2d(x, y));
      }
    }
    return cells;
  }

  toString = (): string => {
    const maxLength = this.getPositions()
      .map(position => this.getCell(position).toString().length)
      .reduce((previous, current) => Math.max(previous, current));

    const lines = [];
    for (let y = 0; y < this.size.y; y++) {
      const cells = [];
      for (let x = 0; x < this.size.x; x++) {
        cells.push(
          padLeft(
            this.getCell(new Vector2d(x, y)).toString(),
            maxLength,
            new Char(" ")
          )
        );
      }
      lines.push(cells.join(" "));
    }
    return lines.join("\n");
  };
}

// Fill up value on the left until it reaches a maximum length
const padLeft = (value: string, maxLength: number, padding: Char): string => {
  return padding.toString().repeat(maxLength - value.length) + value;
};
