const utcZones = ["UTC", "Etc/UTC", "Etc/GMT", "GMT"];

export const showLocalTimesImpl = clock => () => {
    requestAnimationFrame(() => {
        const zone = new Intl.DateTimeFormat().resolvedOptions().timeZone;
        if (utcZones.includes(zone)) return;
        const now = new Date();
        const zoneName = new Intl.DateTimeFormat("en", { timeZoneName: "long" })
            .formatToParts(now).find(part => part.type === "timeZoneName")?.value ?? zone;
        // A time already shown carries its instant in `datetime`.
        for (const time of document.querySelectorAll(".guide time:not([datetime])")) {
            const [hours, minutes] = time.textContent.split(":").map(Number);
            const date = new Date(Date.UTC(
                now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate(), hours, minutes));
            time.dateTime = date.toISOString();
            time.textContent = clock(date.getHours() * 60 + date.getMinutes());
        }
        for (const label of document.querySelectorAll(".guide [data-local]")) {
            label.textContent = label.dataset.local.replace("{zone}", zoneName);
            label.removeAttribute("data-local");
        }
    });
};
