// One game's data from data/<handle>.js and what both prototypes derive from
// it: the brief's roles for today's game fields, time, posts, matching, the card
// a post makes, and the editor for each field.

const GAME = FEED_DATA;
// The dump's date stands in for now, so freshness reads as it did that day.
const NOW = new Date(GAME.now);
const DAY = 864e5;
const BATCH = 20;

// Which of today's game fields play the parts the brief gives them. Games not
// listed have no rank, roles or Looking for, and show every field under Details.
const GAME_ROLES = {
    valorant: { rank: "rank", roles: "role", lookingFor: "interest", groupSize: 5 },
    lol: { rank: "rank", roles: "role", lookingFor: "interest", groupSize: 5 },
    valheim: { lookingFor: "server-focus", server: true },
};
const ROLES = GAME_ROLES[GAME.handle] || { groupSize: 5 };

const LANGUAGE_CODES = {
    English: "EN", German: "DE", Russian: "RU", French: "FR", Hindi: "HI", Spanish: "ES",
    Filipino: "FIL", Polish: "PL", Dutch: "NL", Arabic: "AR", Portuguese: "PT", Swedish: "SV",
    Romanian: "RO", Czech: "CS", Italian: "IT", Ukrainian: "UK", Danish: "DA", Turkish: "TR",
    Hungarian: "HU", Serbian: "SR", Mandarin: "ZH", Greek: "EL", Croatian: "HR", Malay: "MS",
    Slovak: "SK", Japanese: "JA", Norwegian: "NO", Indonesian: "ID", Bengali: "BN", Finnish: "FI",
    Lithuanian: "LT", Bosnian: "BS", Cantonese: "YUE", Persian: "FA", Thai: "TH", Vietnamese: "VI",
    Bulgarian: "BG", Urdu: "UR", Korean: "KO", Latvian: "LV", Estonian: "ET", Slovenian: "SL",
    Albanian: "SQ", Macedonian: "MK", Hebrew: "HE", Tagalog: "TL", Tamil: "TA", Punjabi: "PA",
};
const languageCode = name => LANGUAGE_CODES[name] || name;

const REGIONS = ["Europe", "North America", "South America", "Asia", "Africa", "Oceania"];
const REGION_SHORT = {
    "Europe": "EU", "North America": "NA", "South America": "SA",
    "Asia": "Asia", "Africa": "Africa", "Oceania": "OCE",
};
const CONTINENT_OF = Object.fromEntries(GAME.locations.map(l => [l.name, l.continent]));

const TYPE_CHOICES = [
    { type: "player", icon: "user", text: "I'm a player looking for a group" },
    { type: "group", icon: "users", text: "We're a group looking for players" },
    { type: "community", icon: "castle", text: "We're a community looking for members" },
];

// Game fields.

const gameField = key => GAME.fields.find(f => f.key === key);
const gameOptions = key => (key && gameField(key) ? gameField(key).options : [])
    .map(o => ({ value: o.key, label: o.label }));
const optionLabel = (key, value) => {
    const option = gameOptions(key).find(o => o.value === value);
    return option ? option.label : value;
};
const rankIndex = value => gameOptions(ROLES.rank).findIndex(o => o.value === value);
const rankLabel = index => (gameOptions(ROLES.rank)[index] || {}).label;
const valuesOf = (raw, key) => (key && raw.fields && raw.fields[key]) || [];

// Time. Online hours are stored in the owner's timezone and shown in the
// viewer's (brief 5).

const VIEWER_TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;

const offsetOf = timeZone => {
    try {
        const name = new Intl.DateTimeFormat("en-US", { timeZone, timeZoneName: "longOffset" })
            .formatToParts(NOW).find(p => p.type === "timeZoneName").value;
        const m = name.match(/GMT([+-])(\d\d):(\d\d)/);
        return m ? (m[1] === "-" ? -1 : 1) * (Number(m[2]) * 60 + Number(m[3])) : 0;
    } catch {
        return 0;
    }
};

const toMinutes = hhmm => {
    const [h, m] = hhmm.split(":").map(Number);
    return h * 60 + m;
};

const wrap = minutes => ((minutes % 1440) + 1440) % 1440;

const inViewerTime = (from, to, timeZone) => {
    const shift = offsetOf(VIEWER_TZ) - offsetOf(timeZone || VIEWER_TZ);
    return { from: wrap(toMinutes(from) + shift), to: wrap(toMinutes(to) + shift) };
};

// Clock times follow the viewer's locale, "19:00" or "7pm", with no timezone
// name: the viewer knows their own.
const TWELVE_HOUR = ["h11", "h12"].includes(
    new Intl.DateTimeFormat(undefined, { hour: "numeric" }).resolvedOptions().hourCycle);

const clock = minutes => {
    const h = Math.floor(minutes / 60);
    const m = String(minutes % 60).padStart(2, "0");
    if (!TWELVE_HOUR) return `${String(h).padStart(2, "0")}:${m}`;
    return `${h % 12 || 12}${m === "00" ? "" : `:${m}`}${h < 12 ? "am" : "pm"}`;
};

const formatHours = hours =>
    hours.from === hours.to ? "Online all day" : `${clock(hours.from)}–${clock(hours.to)}`;

// The hours a range covers, crossing midnight where it does. A range that ends
// where it starts covers the whole day.
const hoursCovered = ({ from, to }) => {
    const start = Math.floor(from / 60);
    const end = Math.ceil(to / 60) % 24;
    const covered = new Set();
    let h = start;
    do {
        covered.add(h);
        h = (h + 1) % 24;
    } while (h !== end);
    return covered;
};

const hoursOverlap = (a, b) => {
    const covered = hoursCovered(a);
    return [...hoursCovered(b)].some(h => covered.has(h));
};

const ago = date => {
    const minutes = Math.max(1, Math.round((NOW - date) / 60000));
    const unit = (n, name) => `${n} ${name}${n === 1 ? "" : "s"} ago`;
    if (minutes < 60) return unit(minutes, "minute");
    const hours = Math.round(minutes / 60);
    if (hours < 24) return unit(hours, "hour");
    const days = Math.round(hours / 24);
    if (days < 14) return unit(days, "day");
    if (days < 60) return unit(Math.round(days / 7), "week");
    if (days < 365) return unit(Math.round(days / 30), "month");
    return unit(Math.round(days / 365), "year");
};

// Posts. Today's rows become the brief's post types.

const lifetime = type => (type === "community" ? 90 : 30) * DAY;

const contactOf = raw => {
    if (raw.type === "community") {
        if (raw.has_discord_server) return { label: "Join Discord", icon: "discord" };
        if (raw.has_website) return { label: "Visit site", icon: "external-link" };
        return { label: "Message", icon: "message-circle" };
    }
    return raw.has_discord
        ? { label: "Add on Discord", icon: "discord" }
        : { label: "Message", icon: "message-circle" };
};

// Today's teams record no size, so a group's slots are made up from its id.
const slotsOf = raw => {
    const n = Number(raw.id.slice(1));
    if (ROLES.server) return { wants: `${1 + (n % 2)}–${3 + (n % 2)}` };
    return { members: 1 + (n % 3), total: ROLES.groupSize };
};

const normalize = raw => {
    const updated = new Date(raw.updated);
    const expired = updated.getTime() + lifetime(raw.type) <= NOW.getTime();
    const ranks = valuesOf(raw, ROLES.rank).map(rankIndex).filter(i => i >= 0);
    const shownKeys = raw.type === "community"
        ? [ROLES.lookingFor]
        : [ROLES.rank, ROLES.roles, ROLES.lookingFor];
    const otherFields = GAME.fields
        .filter(f => !shownKeys.includes(f.key) && valuesOf(raw, f.key).length)
        .map(f => ({ label: f.label, value: valuesOf(raw, f.key).map(v => optionLabel(f.key, v)).join(", ") }));
    const about = (raw.about || "").trim();
    const ambitions = (raw.ambitions || "").trim();
    const text = ambitions && !about.includes(ambitions) ? [about, ambitions].filter(Boolean).join("\n\n") : about;
    return {
        id: raw.id,
        type: raw.type,
        name: raw.name,
        owner: raw.owner,
        updated,
        expired,
        // Players don't know when posts expire, so every card counts from its last
        // renewal or edit, and the divider says which have expired (brief 9).
        freshness: `Active ${ago(updated)}`,
        age: raw.age || undefined,
        ageFrom: raw.age_from || undefined,
        ageTo: raw.age_to || undefined,
        location: raw.location,
        region: raw.region,
        regions: raw.regions || [],
        languages: raw.languages || [],
        hours: raw.online_from && raw.online_to ? inViewerTime(raw.online_from, raw.online_to, raw.timezone) : null,
        mic: !!raw.microphone,
        rank: raw.type === "player" && ranks.length ? ranks[0] : undefined,
        rankRange: raw.type !== "player" && ranks.length ? [Math.min(...ranks), Math.max(...ranks)] : undefined,
        roles: valuesOf(raw, ROLES.roles),
        lookingFor: valuesOf(raw, ROLES.lookingFor),
        otherFields,
        text,
        returning: raw.new_or_returning,
        organized: raw.organized,
        contact: contactOf(raw),
        slots: raw.type === "group" ? slotsOf(raw) : undefined,
    };
};

const POSTS = GAME.players.concat(GAME.teams).map(normalize);

const languageOptions = (() => {
    const counts = {};
    POSTS.forEach(p => p.languages.forEach(l => { counts[l] = (counts[l] || 0) + 1; }));
    return Object.keys(counts).sort((a, b) => counts[b] - counts[a]).map(l => ({ value: l, label: l }));
})();
const locationOptions = GAME.locations.map(l => ({ value: l.name, label: l.name }));
const regionOptions = REGIONS.map(r => ({ value: r, label: r }));
const hourOptions = Array.from({ length: 24 }, (_, h) =>
    ({ value: `${String(h).padStart(2, "0")}:00`, label: clock(h * 60) }));

// The bar carries only the fields matching compares for the type (7.1), the
// most used first and the rest under More.
const descriptionFields = type => {
    const lookingFor = ROLES.lookingFor && { key: "lookingFor", label: "Looking for", kind: "multi", options: gameOptions(ROLES.lookingFor) };
    const languages = { key: "languages", label: "Languages", kind: "multi", options: languageOptions };
    const regions = { key: "regions", label: "Regions", kind: "multi", options: regionOptions };
    const hours = { key: "hours", label: "Usually online", kind: "hours", more: true };
    const fields = {
        player: [
            ROLES.rank && { key: "rank", label: "Rank", kind: "single", options: gameOptions(ROLES.rank) },
            ROLES.roles && { key: "roles", label: "Roles", kind: "multi", options: gameOptions(ROLES.roles) },
            { key: "location", label: "Location", kind: "select", options: locationOptions },
            languages,
            { key: "age", label: "Age", kind: "number" },
            lookingFor && { ...lookingFor, more: true },
            hours,
            { key: "mic", label: "Microphone", kind: "toggle", toggleLabel: "I use a microphone", more: true },
        ],
        group: [
            ROLES.roles && { key: "roles", label: "Roles you need", kind: "multi", options: gameOptions(ROLES.roles) },
            ROLES.rank && { key: "rankRange", label: "Rank range", kind: "rankRange", options: gameOptions(ROLES.rank) },
            regions,
            languages,
            { key: "ageRange", label: "Ages", kind: "ageRange" },
            lookingFor && { ...lookingFor, more: true },
            hours,
            { key: "mic", label: "Microphone", kind: "toggle", toggleLabel: "Microphone required", more: true },
        ],
        community: [regions, languages, lookingFor],
    };
    return fields[type].filter(Boolean);
};

const isEmpty = value =>
    value === undefined || value === null || value === "" || value === false
    || (Array.isArray(value) && value.length === 0)
    || (typeof value === "object" && !Array.isArray(value) && Object.values(value).every(isEmpty));

const shortList = labels => labels.length > 2 ? `${labels.slice(0, 2).join(", ")} +${labels.length - 2}` : labels.join(", ");

const summary = (f, value) => {
    if (isEmpty(value)) return null;
    switch (f.kind) {
        case "single": return f.options.find(o => o.value === value).label;
        case "select": return value;
        case "multi":
            if (f.key === "languages") return value.map(languageCode).join(", ");
            if (f.key === "regions") return value.map(r => REGION_SHORT[r]).join(", ");
            return shortList(value.map(v => f.options.find(o => o.value === v).label));
        case "number": return `Age ${value}`;
        case "hours": return value.from && value.to ? `${clock(toMinutes(value.from))}–${clock(toMinutes(value.to))}` : null;
        case "toggle": return f.label;
        case "rankRange": {
            const label = v => f.options.find(o => o.value === v).label;
            if (value.from && value.to) return value.from === value.to ? label(value.from) : `${label(value.from)} – ${label(value.to)}`;
            return value.from ? `${label(value.from)} and up` : `Up to ${label(value.to)}`;
        }
        case "ageRange":
            if (value.from && value.to) return `Ages ${value.from}–${value.to}`;
            return value.from ? `Ages ${value.from}+` : `Ages up to ${value.to}`;
    }
};

// Matching (brief 7.2). Each field the viewer filled in that applies to the
// post's type comes out "fit", "miss", or "missing" when the post left it empty,
// which counts as a miss. A field the viewer left empty isn't compared.

const fit = condition => (condition ? "fit" : "miss");
const overlaps = (a, b) => a.some(x => b.includes(x));
const within = (x, from, to) => x >= (from ?? -Infinity) && x <= (to ?? Infinity);
const has = value => !isEmpty(value);

const compare = (post, type, d) => {
    const m = {};
    const check = (key, viewerGave, postGave, fits) => {
        if (viewerGave) m[key] = postGave ? fit(fits()) : "missing";
    };
    const hoursGiven = has(d.hours) && d.hours.from && d.hours.to;
    const viewerHours = () => ({ from: toMinutes(d.hours.from), to: toMinutes(d.hours.to) });
    const hours = () => check("hours", hoursGiven, post.hours, () => hoursOverlap(viewerHours(), post.hours));
    const shared = () => {
        check("languages", has(d.languages), post.languages.length, () => overlaps(d.languages, post.languages));
        check("lookingFor", has(d.lookingFor), post.lookingFor.length, () => overlaps(d.lookingFor, post.lookingFor));
    };

    if (type === "player") {
        const rank = has(d.rank) ? rankIndex(d.rank) : undefined;
        const continent = has(d.location) ? CONTINENT_OF[d.location] : undefined;
        if (post.type === "player") {
            check("rank", rank !== undefined, post.rank !== undefined, () => Math.abs(rank - post.rank) <= 1);
            check("roles", has(d.roles), post.roles.length, () => overlaps(d.roles, post.roles));
            check("location", continent, post.region, () => continent === post.region);
            check("age", has(d.age), post.age, () => Math.abs(Number(d.age) - post.age) <= 3);
            hours();
            if (d.mic) m.mic = fit(post.mic);
        } else if (post.type === "group") {
            check("needs", has(d.roles), post.roles.length, () => overlaps(d.roles, post.roles));
            check("rank", rank !== undefined, post.rankRange, () => within(rank, ...post.rankRange));
            check("location", continent, post.regions.length, () => post.regions.includes(continent));
            check("ages", has(d.age), post.ageFrom || post.ageTo, () => within(Number(d.age), post.ageFrom, post.ageTo));
            hours();
        } else {
            check("location", continent, post.regions.length, () => post.regions.includes(continent));
        }
        shared();
    } else if (post.type === "player") {
        if (type === "group") {
            check("roles", has(d.roles), post.roles.length, () => overlaps(d.roles, post.roles));
            check("rank", has(d.rankRange), post.rank !== undefined, () => within(post.rank,
                d.rankRange.from ? rankIndex(d.rankRange.from) : undefined,
                d.rankRange.to ? rankIndex(d.rankRange.to) : undefined));
            check("age", has(d.ageRange), post.age, () =>
                within(post.age, Number(d.ageRange.from) || undefined, Number(d.ageRange.to) || undefined));
            hours();
            if (d.mic) m.mic = fit(post.mic);
        }
        check("location", has(d.regions), post.region, () => d.regions.includes(post.region));
        shared();
    }

    const marks = Object.values(m);
    m.compared = marks.length;
    m.fits = marks.filter(v => v === "fit").length;
    m.misses = marks.length - m.fits;
    return m;
};

// Cards: a post with its marks, in the shape prototype.js renders.

const labelsOf = (key, values) => values.map(v => optionLabel(key, v));

const agesText = (from, to) =>
    from && to ? `Ages ${from}–${to}` : from ? `Ages ${from}+` : `Ages up to ${to}`;

const missingText = (key, post) => ({
    rank: "Rank", roles: "Roles", needs: "Roles needed", languages: "Languages",
    hours: "Online hours", age: "Age", ages: "Ages", lookingFor: "Looking for",
    location: post.type === "player" ? "Location" : "Regions",
})[key] + " not given";

const shownMatch = mark => (mark === "fit" || mark === "miss" ? mark : undefined);

const toCard = (post, m) => {
    const facts = [];
    const match = key => shownMatch(m[key]);
    // Every fact has its place in the line. A field the viewer filled in and the
    // post left empty shows in that same place, as not given.
    const slot = (key, given, f) => {
        if (given) facts.push({ ...f, match: key ? match(key) : undefined });
        else if (key && m[key] === "missing") facts.push({ text: missingText(key, post), match: "miss" });
    };
    const regions = post.regions.map(r => REGION_SHORT[r] || r).join(", ");
    const rankRange = post.rankRange && (post.rankRange[0] === post.rankRange[1]
        ? rankLabel(post.rankRange[0])
        : `${rankLabel(post.rankRange[0])} – ${rankLabel(post.rankRange[1])}`);

    if (post.type === "player") {
        slot("rank", post.rank !== undefined, { text: rankLabel(post.rank) });
        slot("roles", post.roles.length, { text: labelsOf(ROLES.roles, post.roles).join(", ") });
        slot("location", post.location, { text: post.location });
    } else if (post.type === "group") {
        slot("rank", post.rankRange, { text: rankRange });
        slot("needs", post.roles.length, { text: `Needs ${labelsOf(ROLES.roles, post.roles).join(", ")}` });
        slot("location", post.regions.length, { text: regions });
    } else {
        slot(null, post.kind, { text: post.kind });
        slot("location", post.regions.length, { text: regions });
    }
    slot("languages", post.languages.length, { text: post.languages.map(languageCode).join(", ") });
    slot(null, post.platforms && post.platforms.length, { text: (post.platforms || []).join(", ") });
    if (post.mic && post.type !== "community") {
        facts.push({ icon: "mic", label: post.type === "group" ? "Microphone required" : "Microphone", match: match("mic") });
    } else if (m.mic === "miss") {
        facts.push({ icon: "mic-off", label: "No microphone", match: "miss" });
    }
    if (post.type === "group") slot("ages", post.ageFrom || post.ageTo, { text: agesText(post.ageFrom, post.ageTo) });
    slot("lookingFor", post.lookingFor.length, { text: labelsOf(ROLES.lookingFor, post.lookingFor).join(", ") });
    slot(null, post.experience, { text: post.experience });
    slot(null, post.returning, { text: "Returning player" });
    slot(null, post.organized && post.type === "group", { text: "Organized" });
    // Online hours and a player's age wait behind Details, and join the end of the
    // line only while compared.
    if (post.type !== "community") {
        slot("hours", post.hours && m.hours, { text: post.hours && formatHours(post.hours), tabular: true });
    }
    if (post.type === "player") slot("age", post.age && m.age, { text: `Age ${post.age}` });

    const details = post.otherFields.slice();
    if (post.type !== "community" && post.hours) {
        const allDay = post.hours.from === post.hours.to;
        details.unshift({ label: "Usually online", value: allDay ? "All day" : formatHours(post.hours) });
    }
    if (post.type === "player" && post.age) details.unshift({ label: "Age", value: String(post.age) });

    const long = post.text.length > (post.type === "community" ? 360 : 170) || post.text.split("\n").length > 2;

    return {
        id: post.id,
        type: post.type,
        name: post.name,
        owner: post.owner,
        freshness: post.freshness,
        expired: post.expired,
        slots: post.slots,
        facts,
        text: post.text,
        details,
        trackers: post.trackers,
        long,
        contact: post.contact,
    };
};

// Editors: one field's control, used in the desktop popovers and the phone sheet.

const editor = (f, value, prefix) => {
    const name = `${prefix}-${f.key}`;
    const options = (type, checked) => `<div class="options">${f.options.map(o =>
        `<label class="option"><input type="${type}" name="${name}" value="${escapeHtml(o.value)}"${checked(o.value) ? " checked" : ""}>${escapeHtml(o.label)}</label>`
    ).join("")}</div>`;
    const select = (part, list, selected, placeholder) =>
        `<select class="select" data-part="${part}" aria-label="${placeholder}"><option value="">${placeholder}</option>${list.map(o =>
            `<option value="${escapeHtml(o.value)}"${o.value === selected ? " selected" : ""}>${escapeHtml(o.label)}</option>`).join("")}</select>`;
    const number = (part, current, placeholder) =>
        `<input class="input" type="number" inputmode="numeric" min="13" max="99" data-part="${part}" placeholder="${placeholder}" aria-label="${placeholder}" value="${current ?? ""}">`;
    const v = value || {};
    const body = {
        single: () => options("radio", o => o === value),
        multi: () => options("checkbox", o => (value || []).includes(o)),
        select: () => select("value", f.options, value, "Choose a country"),
        number: () => number("value", value, "Your age"),
        hours: () => `<div class="range">${select("from", hourOptions, v.from, "From")}<span class="muted">to</span>${select("to", hourOptions, v.to, "To")}</div>
            <span class="muted" style="font-size: 12px">In your own time. A range can cross midnight.</span>`,
        rankRange: () => `<div class="range">${select("from", f.options, v.from, "Lowest")}<span class="muted">to</span>${select("to", f.options, v.to, "Highest")}</div>`,
        ageRange: () => `<div class="range">${number("from", v.from, "From")}<span class="muted">to</span>${number("to", v.to, "To")}</div>`,
        toggle: () => `<label class="option"><input type="checkbox" data-part="value"${value ? " checked" : ""}>${f.toggleLabel}</label>`,
    }[f.kind]();
    return `<div class="editor" data-editor="${f.key}">${body}</div>`;
};

const readEditor = (element, f) => {
    const part = name => element.querySelector(`[data-part="${name}"]`);
    switch (f.kind) {
        case "single": return (element.querySelector("input:checked") || {}).value;
        case "multi": return [...element.querySelectorAll("input:checked")].map(i => i.value);
        case "select":
        case "number": return part("value").value || undefined;
        case "toggle": return part("value").checked;
        default: return { from: part("from").value || undefined, to: part("to").value || undefined };
    }
};
