"use strict";

exports.create = function (state) {
    return function () {
        return new PopStateEvent("popstate", { state: state })
    }
}
