"use strict";

exports._process = process;

exports._writeRawHTTP = function (right, http, buf, cb) {
  return function () {
    var ret = http._writeRaw(buf)
    if (!ret) {
      http.once('drain', cb(right()))
    } else {
      process.nextTick(cb(right()))
    }
    http._headerSent = true
  }
}

function onFinish(outmsg) {
  outmsg.emit('finish');
}

exports._endRawHTTP = function (right, http, cb) {
  return function () {
    if (http.finished) {
      return false
    }
    http.once('finish', cb(right()))
    var finish = onFinish.bind(undefined, http);
    var ret = http._send('', 'latin1', finish);

    http.finished = true;

    // There is the first message on the outgoing queue, and we've sent
    // everything to the socket.
    if (http.output.length === 0 &&
        http.connection &&
        http.connection._httpMessage === http) {
      http._finish();
    }
  }
}

function getDateCurrent() {
  return new Date()
}
