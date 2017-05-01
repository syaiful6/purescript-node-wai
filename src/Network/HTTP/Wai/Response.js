exports.bufferByteLength = function (buff) {
  return function () {
    return Buffer.byteLength(buff)
  }
}
