class ParseError extends Error {
  constructor({ pos, msg }) {
    super(msg);
    this.pos = pos;
  }
  print() {
    return `Parse Error at position ${this.pos}: ${this.message}`;
  }
}

const getChar = ({ string, pos }) => string[pos];

const debugStrPos = ({ string, pos }) => {
  const repeatStr = (str, n) => (n > 0 ? repeatStr(str, n - 1) + str : '');
  console.log(string);
  console.log(repeatStr(' ', pos) + '|');
  console.log(repeatStr(' ', pos) + pos);
  console.log('');
};

const map = (f, parser) => strPos => {
  const out = parser(strPos);
  return { ...out, result: f(out.result) };
};

const flatMap = (f, parser) => strPos => {
  const out = parser(strPos);
  return f(out);
};

const cat = parsers => strPos => {
  const out = { strPos, result: [] };

  parsers.forEach(parser => {
    const o = parser(out.strPos);
    out.strPos = o.strPos;
    out.result.push(o.result);
  });

  return out;
};

const satisfy = pred => strPos => {
  const char = getChar(strPos);
  if (pred(char)) {
    return { result: char, strPos: nextPos(strPos) };
  } else {
    throw new ParseError({
      pos: strPos.pos,
      msg: 'Does not satisfy the predicate',
    });
  }
};

const eof = strPos => {
  if (strPos.pos >= strPos.string.length) {
    return { strPos };
  } else {
    throw new ParseError({ pos: strPos.pos, msg: `Expected EOF.` });
  }
};

const parseChar = testChar => strPos => {
  try {
    return satisfy(ch => ch === testChar)(strPos);
  } catch (e) {
    const char = getChar(strPos);
    throw new ParseError({
      pos: strPos.pos,
      msg: `Could not match character '${testChar}'`,
    });
  }
};

const noneOf = chars => strPos => {
  try {
    return satisfy(char => chars.indexOf(char) === -1)(strPos);
  } catch (err) {
    throw new ParseError({
      ...err,
      msg: `Should be none of [${chars.map(c => '"' + c + '"').join(', ')}]`,
    });
  }
};

const oneOf = chars => strPos => {
  return satisfy(char => chars.indexOf(char) > -1)(strPos);
};

const many = parser => strPos => {
  const out = parser(strPos);
  many(parser)(out.strPos);
};

const manyTil = (parser, parserEnd) => strPos => {
  try {
    parserEnd(strPos);
    return { strPos, result: [] };
  } catch (_) {
    const out1 = parser(strPos);
    const out2 = manyTil(parser, parserEnd)(out1.strPos);
    return { result: [out1.result, ...out2.result], strPos: out2.strPos };
  }
};

const fails = parser => strPos => {
  try {
    const out = parser(strPos);
    console.log(1);
  } catch (err) {
    return { strPos };
  }
  throw new ParseError({ pos: strPos.pos, msg: `Should fail` });
};

const alt = parsers => strPos => {
  for (const parser of parsers) {
    try {
      return parser(strPos);
    } catch (_) {}
  }
  throw new ParseError({ pos: strPos.pos, msg: `No alternative matches` });
};

module.exports = {
  parseChar,
  noneOf,
  many,
  eof,
  manyTil,
  oneOf,
  map,
  flatMap,
  cat,
  fails,
  ParseError,
  alt,
};
