'use strict';

exports._receiveStream = function () {

  function onReadable(right, stream, state, size, cb) {
    var chunk = null;
    while (null !== (chunk = stream.read()) && size > state.len) {
      state.data.push(chunk);
      state.len += chunk.length;
    }
    if (state.len >= size && !state.resolved) {
      resolveRecv(right, stream, state, cb);
    }
  }

  function resolveRecv(right, stream, state, cb) {
    cb(right(Buffer.concat(state.data, state.len)))();
    cleanUpRecv(state, stream);
  }

  function rejectRecv(left, stream, state, cb, err) {
    cb(left(err))();
    cleanUpRecv(state, stream);
  }

  function cleanUpRecv(state, stream) {
    if (state.resolved) {
      return;
    }
    stream.removeListener('readable', state.onReadable)
    stream.removeListener('error', state.onError)
    stream.removeListener('end', state.onEnd)
    state.data = []
    state.onReadable = null;
    state.onError = null;
    state.onEnd = null;
    state.len = 0;
    state.resolved = true;
  }

  function onError(left, stream, state, cb, err) {
    if (!state.resolved) {
      rejectRecv(left, stream, state, cb, err)
    }
  }

  function onEnd(right, stream, state, cb) {
    stream.__ended = true;
    if (!state.resolved) {
      resolveRecv(right, stream, state, cb)
    }
  }

  function doNothing() {}

  return function (left, right, stream, size, cb) {
    return function () {
      if (stream.__ended === true) {
        cb(right(Buffer.allocUnsafe(0)))()
        return doNothing;
      }
      var state = {
        data: [],
        len: 0,
        onReadable: null,
        onEnd: null,
        onError: null,
        resolved: false
      };

      state.onReadable = onReadable.bind(null, right, stream, state, size, cb);
      state.onEnd = onEnd.bind(null, right, stream, state, cb);
      state.onError = onError.bind(null, left, stream, state, cb);

      stream.on('readable', state.onReadable);
      stream.on('error', state.onError);
      stream.on('end', state.onEnd);

      return cleanUpRecv.bind(state, stream);
    };
  };
}();
