"use strict";

exports.writeTextImpl = function (errorCallback) {
    return function (successCallback) {
        return function (text) {
            return function () {
                navigator.clipboard.writeText(text).then(
                    function() {
                        successCallback()
                    },
                    function(error) {
                        errorCallback(error)
                    }
                )
            }
        }
    }
}
