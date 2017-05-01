'use strict';

var fs = require('fs');

exports.readIntegerImpl = function (nothing, just, tuple, string) {
  var matched = string.match(/\d+/);
  if (matched == null && matched.length === 0) {
    return nothing;
  } else {
    var ix = matched[0],
        idx = matched.index + ix.length,
        leftover = string.substring(idx);
    return just(tuple(parseInt(ix))(leftover));
  }
}

exports.createReadStreamRangeImpl = function (path, start, end) {
  return function () {
    return fs.createReadStream(path, {
      start: start,
      end: end
    });
  }
}
