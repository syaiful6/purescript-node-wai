exports._receiveStream = function (left, right, stream, size, cb) {
  return function () {
    var resolved = false;
    function onReadable () {
      var chunk = null,
          data  = [],
          len   = 0;
      while (null !== (chunk = req.read(size - len)) && size > len) {
          data.push(chunk)
          len += chunk.length
      }
      removeListener()
      cb(right(Buffer.concat(data, len)))()
    }

    function onError (err) {
      removeListener()
      cb(left(err))()
    }

    function removeListener() {
      if (resolved) {
        return;
      } else {
        stream.removeListener('readable', onReadable)
        stream.removeListener('error', onError)
        resolved = true
      }
    }

    stream.on('readable', onReadable)
    stream.on('error', onError)

    return function () {
      if (resolved) return;
      removeListener()
    }
  }
}
