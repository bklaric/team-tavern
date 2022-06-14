export const getClientTimezoneImpl = function () {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
}
