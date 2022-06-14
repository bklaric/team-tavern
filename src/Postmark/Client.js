import * as postmark from "postmark"

export const create = function (serverKey) {
    return function () {
        return new postmark.ServerClient(serverKey)
    }
}

export const sendEmailImpl = function (message) {
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
