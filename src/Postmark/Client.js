"use strict";

var postmark = require("postmark")

exports.create = function (serverKey) {
    return function () {
        return new postmark.ServerClient(serverKey)
    }
}

exports.sendEmailImpl = function (message) {
    return function (errorCallback) {
        return function (successCallback) {
            return function (client) {
                return function () {
                    client.sendEmail(message, function (error, result) {
                        if (error) {
                            errorCallback(error)()
                        }
                        else {
                            successCallback(result)()
                        }
                    })
                }
            }
        }
    }
}
