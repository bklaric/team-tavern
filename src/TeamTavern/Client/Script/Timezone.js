export const getClientTimezoneImpl = function () {
    const timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
    if (timeZone == "Europe/Kyiv") {
        return "Europe/Kiev"
    }
    else {
        return timeZone
    }
}
