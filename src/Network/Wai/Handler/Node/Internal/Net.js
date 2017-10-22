'use strict';

var net = require('net');

exports.createServer = function (listener) {
  return function () {
    return net.createServer(function (socket) {
      listener(socket)()
    });
  };
};

exports.listenImpl = function (server) {
  return function (options) {
    return function (done) {
      return function () {
        if (options.backlog !== null) {
          server.listen(options.port, options.hostname, options.backlog, done);
        } else {
          server.listen(options.port, options.hostname, done)
        }
      };
    };
  };
};

exports.listenSocket = function (server) {
  return function (path) {
    return function (done) {
      return function () {
        server.listen(path, backlog, done)
      };
    };
  };
};

exports.destroyImpl = function (err) {
  return function (socket) {
    return function () {
      if (err !== null) {
        socket.destroy(err)
      } else {
        socket.destroy()
      }
    }
  }
}
