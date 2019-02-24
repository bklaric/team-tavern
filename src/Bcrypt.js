"use strict";

var bcrypt = require('bcrypt');

exports.genSaltImpl = function (rounds) {
    return function (minor) {
        return function (errorCallback) {
            return function (successCallback) {
                return function () {
                    bcrypt.genSalt(rounds, minor, function (error, salt) {
                        if (error) {
                            errorCallback(error)()
                        }
                        else {
                            successCallback(salt)()
                        }
                    })
                }
            }
        }
    }
}

exports.hashImpl = function (data) {
    return function (saltOrRounds) {
        return function (errorCallback) {
            return function (successCallback) {
                return function () {
                    bcrypt.hash(data, saltOrRounds, function (error, hash) {
                        if (error) {
                            errorCallback(error)()
                        }
                        else {
                            successCallback(hash)()
                        }
                    })
                }
            }
        }
    }
}

exports.compareImpl = function (data) {
    return function (encrypted) {
        return function (errorCallback) {
            return function (successCallback) {
                return function () {
                    bcrypt.compare(data, encrypted, function (error, hash) {
                        if (error) {
                            errorCallback(error)()
                        }
                        else {
                            successCallback(success)()
                        }
                    })
                }
            }
        }
    }
}

exports.getRounds = function (encrypted) {
    return bcrypt.getRounds(encrypted)
}
