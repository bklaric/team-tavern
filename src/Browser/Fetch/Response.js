"use strict";

exports.status = function (response) {
    return response.status
}

exports.textImpl = function (successCallback) {
    return function (response) {
        return function () {
            response.text().then(
                function (body) { successCallback(body)() }
            )
        }
    }
}

exports.jsonImpl = function (successCallback) {
    return function (errorCallback){
        return function (response) {
            return function () {
                response.json().then(
                    function (body) { successCallback(body)() },
                    function (error) { errorCallback(error)() }
                )
            }
        }
    }
}
