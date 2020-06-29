"use strict";

exports.getClientTimezone = function () {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
}
