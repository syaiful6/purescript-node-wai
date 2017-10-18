'use strict'

exports.pipeStreamNoEndEff = function (left, right, rs, ws, cb) {
  return function () {
    rs.pipe(ws, { end: false });
    rs.on('error', function (err) {
      cb(left(err))();
    });
    rs.on('end', function () {
      cb(right({}))();
    });
  }
}
