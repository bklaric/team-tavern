// One game's data from data/<handle>.js and what both prototypes derive from
// it: the brief's roles for today's game fields, time, posts, matching, the card
// a post makes, and the editor for each field.

const GAME = FEED_DATA;
const BATCH = 20;

// Which of today's game fields play the parts the brief gives them. Games not
// listed have no rank, roles or Looking for, and show every field under Details.
const GAME_ROLES = {
    apex: { rank: "battle-royale-rank", lookingFor: "interest", groupSize: 3 },
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

// Platforms. Today's are stores as much as devices; the brief's platform is what
// a player plays on, so every PC store is one option. A game on one platform has
// no platform field (brief 5).
const PLATFORM_OF = {
    steam: "pc", origin: "pc", riot: "pc", "battle.net": "pc", "ubisoft-connect": "pc",
    playstation: "playstation", xbox: "xbox", switch: "switch",
};
const PLATFORM_LABELS = { pc: "PC", playstation: "PlayStation", xbox: "Xbox", switch: "Switch" };
const platformsOf = stored => [...new Set(stored.map(p => PLATFORM_OF[p]))];
const platformOptions = (platforms => platforms.length > 1
    ? platforms.map(p => ({ value: p, label: PLATFORM_LABELS[p] }))
    : [])(platformsOf(GAME.platforms || []));
const knownPlatforms = values => platformOptions.map(o => o.value).filter(p => (values || []).includes(p));

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

// Contacts. How the owner wants to be reached sets the card's button (brief
// 5.6); the contacts are what the contact panel shows.
const contactButtonOf = (reach, contacts) => {
    if (reach === "discord") return { label: "Join Discord", icon: "discord" };
    if (reach === "website") return { label: "Visit site", icon: "external-link" };
    if (reach === "offsite") {
        return contacts.some(c => c.label === "Discord")
            ? { label: "Add on Discord", icon: "discord" }
            : { label: "Add in game", icon: "gamepad-2" };
    }
    if (reach === "either") return { label: "Contact", icon: "message-circle" };
    return { label: "Message", icon: "message-circle" };
};

const asUrl = value => (/^https?:\/\//.test(value) ? value : `https://${value}`);
const linkContact = (label, value) => ({ label, value, url: asUrl(value) });

// Today's rows say whether a contact exists, not what it is: the dump's handles
// stay out of the sample. The prototype makes one up from the name.
const handleOf = name => name.toLowerCase().replace(/[^a-z0-9._]+/g, "") || "player";

const reachOf = raw => raw.type === "community"
    ? (raw.has_discord_server ? "discord" : raw.has_website ? "website" : "message")
    : (raw.has_discord ? "offsite" : "message");

const contactsOf = raw => {
    const slug = handleOf(raw.name || raw.owner);
    return [
        raw.type !== "community" && raw.has_discord && { label: "Discord", value: handleOf(raw.owner) },
        raw.type !== "player" && raw.has_discord_server
            && linkContact(raw.type === "community" ? "Discord invite" : "Discord server", `discord.gg/${slug}`),
        raw.type !== "player" && raw.has_website && linkContact("Website", `${slug}.gg`),
    ].filter(Boolean);
};

// Posts. Today's rows become the brief's post types.

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
        platforms: knownPlatforms(platformsOf(raw.platforms || [])),
        otherFields,
        text,
        returning: raw.new_or_returning,
        organized: raw.organized,
        reach: reachOf(raw),
        contacts: contactsOf(raw),
        contact: contactButtonOf(reachOf(raw), contactsOf(raw)),
        slots: raw.type === "group" ? slotsOf(raw) : undefined,
    };
};

// Posts from drafts: what the post screen writes, and what the prototype's
// accounts have published.

// What the brief makes game fields that today's data lacks, made up for the
// prototype: the game account players add each other by and a community's
// kinds. Trackers are the seed's: each links a profile built from the game
// account.
const GAME_EXTRAS = {
    apex: {
        account: { key: "ea", label: "EA ID", placeholder: "Your EA ID" },
        trackers: [
            { title: "tracker.gg", url: id => `https://tracker.gg/apex/profile/origin/${encodeURIComponent(id)}` },
        ],
        kinds: ["Discord server", "Clan", "Esports organization"],
    },
    valorant: {
        account: { key: "riot", label: "Riot ID", placeholder: "Name#TAG" },
        trackers: [
            { title: "tracker.gg", url: id => `https://tracker.gg/valorant/profile/riot/${encodeURIComponent(id)}` },
            { title: "blitz.gg", url: id => `https://blitz.gg/valorant/profile/${id.replace("#", "-")}` },
        ],
        kinds: ["Discord server", "Clan", "Esports organization"],
    },
    lol: {
        account: { key: "riot", label: "Riot ID", placeholder: "Name#TAG" },
        kinds: ["Discord server", "Clan", "Esports organization"],
    },
    valheim: {
        account: { key: "steam", label: "Steam profile", placeholder: "steamcommunity.com/id/…" },
        kinds: ["Dedicated server", "Discord server", "Clan"],
    },
};
const EXTRAS = GAME_EXTRAS[GAME.handle] || {
    account: { key: "steam", label: "Steam profile", placeholder: "steamcommunity.com/id/…" },
    kinds: ["Discord server", "Clan"],
};

const rankRangeOf = range => {
    if (!range || (!range.from && !range.to)) return undefined;
    const from = range.from ? rankIndex(range.from) : 0;
    const to = range.to ? rankIndex(range.to) : gameOptions(ROLES.rank).length - 1;
    return [Math.min(from, to), Math.max(from, to)];
};

// The game's fields a post doesn't name by the part they play. A community
// isn't asked for rank or roles, so for one they are among them.
const otherGameFieldsOf = type => {
    const named = type === "community" ? [ROLES.lookingFor] : [ROLES.rank, ROLES.roles, ROLES.lookingFor];
    return GAME.fields.filter(f => !named.includes(f.key));
};

// Player and group posts show their owner's Discord and game account; groups
// and communities their server and site.
const draftContacts = (type, d) => [
    type !== "community" && d.discord && { label: "Discord", value: d.discord },
    type !== "community" && d.gameAccount && { label: EXTRAS.account.label, value: d.gameAccount },
    type !== "player" && d.discordServer && linkContact(type === "community" ? "Discord invite" : "Discord server", d.discordServer),
    type !== "player" && d.website && linkContact("Website", d.website),
].filter(Boolean);

// A draft's post, in the shape toCard takes. Hours are written in the owner's
// timezone; others see them in theirs, the owner as written.
const draftPost = (type, d, owner, updated, { id = "draft", own = false, inTheirTime = false } = {}) => {
    const named = (d.name || "").trim();
    const name = type === "player" ? owner || "You"
        : named || (owner ? undefined : type === "group" ? "Your group" : "Your community");
    const otherFields = otherGameFieldsOf(type).filter(f => has(d[`field:${f.key}`])).map(f => ({
        label: f.label,
        value: [].concat(d[`field:${f.key}`]).map(v => optionLabel(f.key, v)).join(", "),
    }));
    const hours = d.hours && d.hours.from && d.hours.to
        ? (inTheirTime ? inViewerTime(d.hours.from, d.hours.to, d.timezone) : { from: toMinutes(d.hours.from), to: toMinutes(d.hours.to) })
        : null;
    const at = updated ? new Date(updated) : NOW;
    const reach = type === "community" ? d.join || "message" : d.reach || "message";
    const contacts = draftContacts(type, d);
    return {
        id,
        own,
        type,
        name,
        owner,
        updated: at,
        freshness: `Active ${ago(at)}`,
        expired: at.getTime() + lifetime(type) <= NOW.getTime(),
        age: type === "player" ? ageAt(d.birthday) : undefined,
        ageFrom: Number((d.ageRange || {}).from) || undefined,
        ageTo: Number((d.ageRange || {}).to) || undefined,
        location: d.location,
        region: CONTINENT_OF[d.location],
        regions: d.regions || [],
        languages: d.languages || [],
        hours,
        mic: !!d.mic,
        rank: type === "player" && d.rank ? rankIndex(d.rank) : undefined,
        rankRange: type === "group" ? rankRangeOf(d.rankRange) : undefined,
        roles: type === "community" ? [] : d.roles || [],
        lookingFor: d.lookingFor || [],
        otherFields,
        text: (d.text || "").trim(),
        returning: type === "player" && d.returning,
        organized: type === "group" && d.organized,
        kind: type === "community" ? d.kind : undefined,
        experience: type === "community" ? d.experience : undefined,
        platforms: knownPlatforms(d.platforms),
        reach,
        contacts,
        contact: contactButtonOf(reach, contacts),
        slots: type !== "group" ? undefined
            : ROLES.server ? { wants: d.wantsFrom === d.wantsTo ? `${d.wantsFrom}` : `${d.wantsFrom}–${d.wantsTo}` }
            : { members: d.members, total: d.total },
        trackers: type === "player" && d.gameAccount && EXTRAS.trackers
            ? EXTRAS.trackers.map(t => ({ title: t.title, url: t.url(d.gameAccount) }))
            : [],
    };
};

// Facts about a player and their contacts live on the account, and posts show
// them as the account has them now (brief 6, step 3).
const ACCOUNT_FACTS = {
    player: ["location", "languages", "birthday", "timezone", "discord", "gameAccount"],
    group: ["timezone", "discord", "gameAccount"],
    community: [],
};

const accountFactOf = (person, key) =>
    key === "gameAccount" ? (person.accounts || {})[EXTRAS.account.key] : person[key];

const withPerson = (type, d, person) => ({
    ...d,
    ...Object.fromEntries(ACCOUNT_FACTS[type].map(key => [key, accountFactOf(person, key)]).filter(([, v]) => v !== undefined)),
});

// The prototype's accounts post in the feed too: Kestrel's Night Owls is in
// Valorant's, and whatever the viewer publishes is in its game's.
const accountPosts = () => {
    const people = Object.values(PRESETS).map(p => (account && account.id === p.id ? account : p));
    if (account && !PRESETS[account.id]) people.push(account);
    return people.filter(person => person.nickname).flatMap(person => person.posts
        .filter(p => p.game === GAME.handle && p.draft)
        .map(p => draftPost(p.type, withPerson(p.type, p.draft, person), person.nickname, p.updated, {
            id: postId(person, p),
            own: !!account && person.id === account.id,
            inTheirTime: true,
        })));
};

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
    const platforms = platformOptions.length && { key: "platforms", label: "Platform", kind: "multi", options: platformOptions };
    const hours = { key: "hours", label: "Usually online", kind: "hours", more: true };
    const fields = {
        player: [
            ROLES.rank && { key: "rank", label: "Rank", kind: "single", options: gameOptions(ROLES.rank) },
            ROLES.roles && { key: "roles", label: "Roles", kind: "multi", options: gameOptions(ROLES.roles) },
            platforms,
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
            platforms,
            regions,
            languages,
            { key: "ageRange", label: "Ages", kind: "ageRange" },
            lookingFor && { ...lookingFor, more: true },
            hours,
            { key: "mic", label: "Microphone", kind: "toggle", toggleLabel: "Microphone required", more: true },
        ],
        community: [regions, languages, platforms, lookingFor],
    };
    return fields[type].filter(Boolean);
};

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
        check("platforms", has(d.platforms), post.platforms.length, () => overlaps(d.platforms, post.platforms));
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
    hours: "Online hours", age: "Age", ages: "Ages", lookingFor: "Looking for", platforms: "Platform",
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
    slot("platforms", post.platforms.length, { text: post.platforms.map(p => PLATFORM_LABELS[p]).join(", ") });
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

// Every post in the feed, the accounts' among them, and the languages they use,
// most used first.

const POSTS = GAME.players.concat(GAME.teams).map(normalize).concat(accountPosts());

const languageOptions = (() => {
    const counts = {};
    POSTS.forEach(p => p.languages.forEach(l => { counts[l] = (counts[l] || 0) + 1; }));
    return Object.keys(counts).sort((a, b) => counts[b] - counts[a]).map(l => ({ value: l, label: l }));
})();

// What the contact panel and a conversation keep of a post (messaging.js).

const postInfo = post => ({
    id: post.id,
    game: GAME.handle,
    type: post.type,
    name: post.name,
    owner: post.owner,
    updated: post.updated.toISOString(),
    slots: post.slots,
    facts: toCard(post, {}).facts,
    reach: post.reach,
    contacts: post.contacts,
});

const postInfoById = id => {
    const post = POSTS.find(p => p.id === id);
    return post && postInfo(post);
};

// The viewer's post in this game, which the owner of a post they message sees
// in the conversation's header: their player post, else their group or
// community post.
const viewerPostIn = () => {
    const mine = POSTS.filter(p => p.own);
    const post = ["player", "group", "community"].map(type => mine.find(p => p.type === type)).find(Boolean);
    return post ? { type: post.type, name: post.name, owner: post.owner, facts: toCard(post, {}).facts } : null;
};
