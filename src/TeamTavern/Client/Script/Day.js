// Month names are in the site's language; the order of day and month follows
// the viewer's locale where it is an English one.
const locale = navigator.language.startsWith("en") ? navigator.language : "en-GB";

const startOf = date => new Date(date.getFullYear(), date.getMonth(), date.getDate()).getTime();

export const dayLabelImpl = now => time => {
    const date = new Date(time);
    const days = Math.round((startOf(new Date(now)) - startOf(date)) / (24 * 60 * 60 * 1000));
    if (days <= 0) return "Today";
    if (days === 1) return "Yesterday";
    return date.toLocaleDateString(locale, { day: "numeric", month: "long" });
};

export const minuteOfDay = time => {
    const date = new Date(time);
    return date.getHours() * 60 + date.getMinutes();
};
