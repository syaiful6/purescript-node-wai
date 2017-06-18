'use strict';

var EE = require('events');

exports.removeAllListener = function (src) {
  return function () {
    src.removeAllListeners('data')
    src.removeAllListeners('error')
    src.removeAllListeners('end')
  }
}
