const intlNames = new Map();

export const browserTimezone = () => Intl.DateTimeFormat().resolvedOptions().timeZone;

export const intlName = name => {
    if (!intlNames.has(name)) {
        let resolved = name;
        try {
            resolved = new Intl.DateTimeFormat("en-US", { timeZone: name }).resolvedOptions().timeZone;
        } catch {}
        intlNames.set(name, resolved);
    }
    return intlNames.get(name);
};
