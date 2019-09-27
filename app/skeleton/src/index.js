const fs = require('fs');

const parser = require('./parser.js');

const parseName = strPos => {
  const chars = [' ', '/', '\n'];
  return parser.map(
    chars => chars.join(''),
    parser.map(
      ([x, xs]) => [x, ...xs],
      parser.cat([
        parser.noneOf(chars),
        parser.manyTil(parser.noneOf(chars), parser.oneOf(chars)),
      ]),
    ),
  )(strPos);
};

const parseDirName = strPos => {
  return parser.map(
    ([xs, _]) => xs,
    parser.cat([parseName, parser.parseChar('/')]),
  )(strPos);
};

const parseIndent = level => strPos => {
  const out = parser.manyTil(parser.parseChar(' '), parser.noneOf([' ']))(
    strPos,
  );
  if (out.result.length !== level) {
    throw new parser.ParseError({ pos: strPos.pos, msg: `Wrong indent` });
  } else {
    return { strPos: out.strPos };
  }
};

const parseLine = (level, parser_) => strPos => {
  return parser.map(
    ([_, x]) => x,
    parser.cat([parseIndent(level), parser_, parser.parseChar('\n')]),
  )(strPos);
};

const parseLevel = level => strPos => {
  const result = {};

  const parseDirOrFileName = strPos => {
    try {
      const dirName = parseLine(level, parseDirName)(strPos);
      const sub = parseLevel(level + 1)(dirName.strPos);
      result[dirName.result] = sub.result;
      return sub;
    } catch (_) {
      try {
        const fileName = parseLine(level, parseName)(strPos);
        result[fileName.result] = null;
        return fileName;
      } catch (_) {
        throw new parser.ParseError({
          pos: strPos.pos,
          msg: `Cannot parse directory or file name`,
        });
      }
    }
  };

  return parser.map(
    _ => result,
    parser.manyTil(
      parseDirOrFileName,
      parser.alt([parser.eof, parseIndent(level - 1)]),
    ),
  )(strPos);
};

const run = (parser, str) => {
  try {
    return parser({ pos: 0, string: str });
  } catch (err) {
    if (err.print) {
      console.error(err.print());
    } else {
      throw err;
    }
  }
};

const checkExistance = (path, tree) => {
  Object.keys(tree).forEach(key => {
    const value = tree[key];
    const newPath = [...path, key];
    const pathStr = newPath.join('/');

    if (typeof value === '[object]') {
      checkExistance(newPath, value);
    }

    try {
      fs.statSync(pathStr);
    } catch (_) {
      return;
    }
    throw new Error(`Path ${pathStr} already exists`);
  });
};

const createFilesAndDirs = (path, tree) => {
  Object.keys(tree).forEach(key => {
    const value = tree[key];
    const newPath = [...path, key];
    const pathStr = newPath.join('/');

    if (value === null) {
      fs.writeFileSync(pathStr, '');
    } else if (typeof value === 'object') {
      fs.mkdirSync(pathStr);
      createFilesAndDirs(newPath, value);
    }
  });
};

const main = () => {
  const fileName = process.argv[2];
  const targetDir = process.argv[3];

  const input = fs.readFileSync(fileName).toString();

  const tree = run(parseLevel(0), input);

  try {
    checkExistance([targetDir], tree.result);
  } catch (err) {
    console.error(err.message);
    process.exit(1);
  }

  createFilesAndDirs([targetDir], tree.result);
};

main();
