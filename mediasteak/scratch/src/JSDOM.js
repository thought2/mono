JSDOM = require("jsdom").JSDOM;

exports.getDocument = function (string) {
    return new JSDOM(string).window.document;
}
