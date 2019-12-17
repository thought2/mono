// JavaScript does not have characters, only strings. Small helper.
export default class Char {
  private value: string;
  constructor(value: string) {
    if (value.length !== 1) throw new Error("not a character.");
    this.value = value;
  }

  toString() {
    return this.value;
  }
}
