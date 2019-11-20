var globalObj;

try {
  globalObj = window;
} catch (e) {
  globalObj = global;
}

if (!globalObj.__appState) {
  globalObj.__appState = {};
}

exports.writeCell = function(id) {
  return function(x) {
    return function() {
      globalObj.__appState[id] = x;
    };
  };
};

exports._readCell = function(id) {
  return function() {
    return globalObj.__appState[id];
  };
};
