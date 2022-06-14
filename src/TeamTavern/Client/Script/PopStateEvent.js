export const create = function (state) {
    return function () {
        return new PopStateEvent("popstate", { state: state })
    }
}
