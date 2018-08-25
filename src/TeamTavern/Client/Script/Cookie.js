"use strict";

exports.hasPlayerIdCookie = function () {
    return !!document.cookie.split(';').filter(function (item) {
        return item.indexOf('teamtavern-id=') >= 0
    }).length
}

exports.cookies = function () {
    return document.cookie;
}
