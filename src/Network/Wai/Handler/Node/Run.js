exports.pipeNoEnd = function (read, write) {
  return function () {
    return read.pipe(write, { end : false });
  };
}

exports.closeHttpServer = function (nonCanceler, server) {
  return function (success, reject) {
    server.close(function (err) {
      if (err) {
        reject(err);
      } else {
        success();
      }
    });
    return nonCanceler;
  };
}
