"use strict";

exports.url = function (url) {
    return function() {
        return new URL(url);
    }
}

exports.href = function (url) {
    return function () {
        return url.href;
    }
}

exports.searchParams = function (url) {
    return function() {
        return url.searchParams;
    }
}

exports.append = function (key) {
    return function (value) {
        return function (params) {
            return function() {
                params.append(key, value);
            }
        }
    }
}

exports.set = function (key) {
    return function (value) {
        return function (params) {
            return function () {
                params.set(key, value);
            }
        }
    }
}

exports.getImpl = function (key) {
    return function (params) {
        return function() {
            return params.get(key);
        }
    }
}

exports.getAll = function (key) {
    return function (params) {
        return function() {
            return params.getAll(key);
        }
    }
}

exports.delete = function (key) {
    return function (params) {
        return function() {
            params.delete(key);
        }
    }
}

exports.keys = function (params) {
    return function() {
        return Array.from(params.keys());
    }
}
