import * as bcrypt from "bcrypt"

export const genSaltImpl = function (rounds) {
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

export const hashImpl = function (data) {
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

export const compareImpl = function (data) {
    return function (encrypted) {
        return function (errorCallback) {
            return function (successCallback) {
                return function () {
                    bcrypt.compare(data, encrypted, function (error, result) {
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

export const getRounds = function (encrypted) {
    return bcrypt.getRounds(encrypted)
}
