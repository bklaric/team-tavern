export const fetchImpl = function (url) {
    return function (options) {
        return function (successCallback) {
            return function (errorCallback) {
                return function () {
                    fetch(url, options).then(
                        function (response) { successCallback(response)() },
                        function (error) { errorCallback(error)() }
                    )
                }
            }
        }
    }
}
