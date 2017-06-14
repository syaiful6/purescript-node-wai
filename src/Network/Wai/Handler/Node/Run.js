exports.pipeNoEnd = function (read) {
  return function (write) {
    return function () {
      return read.pipe(write, { end : false });
    };
  };
}