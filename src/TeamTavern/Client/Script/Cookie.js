"use strict";

exports.cookies = function () {
    return document.cookie;
}

exports.deleteCookie = function (key) {
    return function () {
        document.cookie = key + "=0; max-age=0"
    }
}
