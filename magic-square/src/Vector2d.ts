// mutable vectors
export default class Vector2d {
  x: number;
  y: number;
  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  add = (vector: Vector2d): Vector2d => {
    this.x += vector.x;
    this.y += vector.y;
    return this;
  };

  mod = (vector: Vector2d): Vector2d => {
    this.x = mod(this.x, vector.x);
    this.y = mod(this.y, vector.y);
    return this;
  };

  copy = (): Vector2d => {
    return new Vector2d(this.x, this.y);
  };
}

// https://web.archive.org/web/20090717035140if_/javascript.about.com/od/problemsolving/a/modulobug.htm
const mod = (x: number, n: number): number => ((x % n) + n) % n;
