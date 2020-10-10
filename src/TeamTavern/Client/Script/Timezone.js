"use strict";

exports.getClientTimezoneImpl = function () {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
}
