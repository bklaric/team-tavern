export const status = function (response) {
    return response.status
}

export const textImpl = function (successCallback) {
    return function (response) {
        return function () {
            response.text().then(
                function (body) { successCallback(body)() }
            )
        }
    }
}

export const jsonImpl = function (successCallback) {
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
