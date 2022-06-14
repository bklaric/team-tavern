import * as sgMail from "@sendgrid/mail"

export const setApiKey = function (key) {
    return function () {
        sgMail.setApiKey(key)
    }
}

export const sendImpl = function (onError) {
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
