export const offsetOf = now => timeZone => {
    try {
        const name = new Intl.DateTimeFormat("en-US", { timeZone, timeZoneName: "longOffset" })
            .formatToParts(new Date(now)).find(part => part.type === "timeZoneName").value
        const match = name.match(/GMT([+-])(\d\d):(\d\d)/)
        return match ? (match[1] === "-" ? -1 : 1) * (Number(match[2]) * 60 + Number(match[3])) : 0
    } catch {
        return 0
    }
}
