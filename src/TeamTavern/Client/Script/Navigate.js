"use strict";

exports.setTimeout = function (handler) {
    return function (timeout) {
        return function () {
            setTimeout(handler, timeout)
        }
    }
}
