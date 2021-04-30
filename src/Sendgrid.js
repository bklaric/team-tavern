"use strict";

const sgMail = require('@sendgrid/mail');

exports.setApiKey = function (key) {
    return function () {
        sgMail.setApiKey(key)
    }
}

exports.sendImpl = function (onError) {
    return function (onSuccess) {
        return function (email) {
            return function () {
                sgMail.send(email).then(
                    function () { onSuccess() },
                    function (error) { onError(error)() })
            }
        }
    }
}
