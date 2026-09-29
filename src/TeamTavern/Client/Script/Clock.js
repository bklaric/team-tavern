const twelveHour = ["h11", "h12"].includes(
    new Intl.DateTimeFormat(undefined, { hour: "numeric" }).resolvedOptions().hourCycle);

export const clock = minutes => {
    const hours = Math.floor(minutes / 60) % 24;
    const rest = String(minutes % 60).padStart(2, "0");
    if (!twelveHour) {
        return `${String(hours).padStart(2, "0")}:${rest}`;
    }
    return `${hours % 12 || 12}${rest === "00" ? "" : `:${rest}`}${hours < 12 ? "am" : "pm"}`;
};
