export const url = function (url) {
    return function () {
        return new URL(url);
    }
}

export const href = function (url) {
    return function () {
        return url.href;
    }
}

export const searchParams = function (url) {
    return function () {
        return url.searchParams;
    }
}

export const append = function (key) {
    return function (value) {
        return function (params) {
            return function () {
                params.append(key, value);
            }
        }
    }
}

export const set = function (key) {
    return function (value) {
        return function (params) {
            return function () {
                params.set(key, value);
            }
        }
    }
}

export const getImpl = function (key) {
    return function (params) {
        return function () {
            return params.get(key);
        }
    }
}

export const getAll = function (key) {
    return function (params) {
        return function () {
            return params.getAll(key);
        }
    }
}

export const deleteImpl = function (key) {
    return function (params) {
        return function () {
            params.delete(key);
        }
    }
}

export const keys = function (params) {
    return function () {
        return Array.from(params.keys());
    }
}
