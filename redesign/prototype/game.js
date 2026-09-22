// One game's data from data/<handle>.js and what both prototypes derive from
// it: the game's fields, time, posts, matching, the card a post makes, and the
// editor for each field.

const GAME = FEED_DATA;
const BATCH = 20;

const TYPE_CHOICES = [
    { type: "player", icon: "user", text: "I'm a player looking for a group" },
    { type: "group", icon: "users", text: "We're a group looking for players" },
    { type: "community", icon: "castle", text: "We're a community looking for members" },
];

// Game fields. A field says how it is matched and shown (src/TeamTavern/Database/TablesCurrent.sql):
// its ilk, whether its options are ordered or slots on a team, which post types
// ask it, and whether it leads the card.

const FIELDS = GAME.fields;
const gameField = key => FIELDS.find(f => f.key === key);
const fieldsFor = type => FIELDS.filter(f => f.appliesTo.includes(type));
const optionsOf = f => f.options.map(o => ({ value: o.key, label: o.label }));
const optionLabel = (f, value) => (f.options.find(o => o.key === value) || { label: value }).label;
const stepOf = (f, value) => f.options.findIndex(o => o.key === value);

// A post's answers sit under the keys the post screen and the description bar
// write them with: field:<key> for the options chosen, or true for a yes, and
// range:<key> for an ordered field on a group or community, which asks for a
// range where a player gives a point.
const answerKey = (f, type) => `${f.ordered && type !== "player" ? "range" : "field"}:${f.key}`;
const isAnswerKey = key => key.startsWith("field:") || key.startsWith("range:");

const chosenIn = (f, v) => [].concat(v[`field:${f.key}`] || []);
const pointIn = (f, v) => {
    const step = stepOf(f, v[`field:${f.key}`]);
    return step >= 0 ? step : undefined;
};
const rangeIn = (f, v) => {
    const range = v[`range:${f.key}`];
    if (!range || (!range.from && !range.to)) return undefined;
    const from = range.from ? stepOf(f, range.from) : 0;
    const to = range.to ? stepOf(f, range.to) : f.options.length - 1;
    return [Math.min(from, to), Math.max(from, to)];
};
const orderedIn = (f, v, type) => (type === "player" ? pointIn(f, v) : rangeIn(f, v));

// How a field reads: a player's yes is the field itself, a group's or a
// community's is what it needs (brief 5). A slotted field with every option
// picked plays anything.
const article = word => (/^[aeiou]/i.test(word) ? "an" : "a");
const flagText = (f, type, yes = true) => {
    const thing = `${article(f.label)} ${f.label.toLowerCase()}`;
    if (type === "player") return yes ? f.label : `Not ${thing}`;
    return yes ? `Needs ${thing}` : `Doesn't need ${thing}`;
};
const anyText = f => `Any ${f.label.toLowerCase()}`;
const plural = word => (word.endsWith("s") ? `${word}es` : `${word}s`);

// Overwatch ranks each role on a ladder of its own, so where a card leads with
// more than one ladder each names itself.
const LADDERS_NAMED = FIELDS.filter(f => f.ordered && f.onCard).length > 1;

const rangeText = (f, [from, to]) => {
    const label = step => f.options[step].label;
    if (from === to) return label(from);
    if (from === 0) return `Up to ${label(to)}`;
    if (to === f.options.length - 1) return `${label(from)} and up`;
    return `${label(from)} – ${label(to)}`;
};

// A post's answer to a field as the card says it, or undefined where it gave
// none. A no to a boolean is no answer to show.
const answerText = (f, post) => {
    if (f.ilk === "boolean") return post.answers[`field:${f.key}`] ? flagText(f, post.type) : undefined;
    if (f.ordered) {
        const step = orderedIn(f, post.answers, post.type);
        return step === undefined ? undefined : post.type === "player" ? f.options[step].label : rangeText(f, step);
    }
    const chosen = chosenIn(f, post.answers);
    if (!chosen.length) return undefined;
    if (f.slotted && chosen.length === f.options.length) return anyText(f);
    return chosen.map(v => optionLabel(f, v)).join(", ");
};

// Rank closeness (brief 7.2): two players' ranks are near when they are within
// a tier of each other. A tier is the options whose labels differ only in a
// trailing division, Diamond 1 to Diamond 3, and the game's commonest tier is
// how many steps that is: 3 in Valorant, 4 in League, 5 in Overwatch and Dota.
// A ladder with no divisions, such as TF2's or Faceit's, counts one step. The
// prototype bar sets it to one step everywhere, to compare.
const NEAR_KEY = "tt-proto-near";
const nearByTier = () => localStorage.getItem(NEAR_KEY) !== "step";
const tierName = label => label.replace(/\s+(\d+|[IVX]+)$/, "");
const tierSteps = f => {
    const sizes = {};
    f.options.forEach(o => { sizes[tierName(o.label)] = (sizes[tierName(o.label)] || 0) + 1; });
    const counts = Object.values(sizes);
    if (counts.length === 1) return 1;
    const often = {};
    counts.forEach(n => { often[n] = (often[n] || 0) + 1; });
    return Number(Object.keys(often).sort((a, b) => often[b] - often[a] || b - a)[0]);
};
const nearSteps = f => (nearByTier() ? tierSteps(f) : 1);

// Time. Online hours are stored in the owner's timezone and shown in the
// viewer's (brief 5).

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

// The sample says whether a contact exists, not what it is: the dump's handles
// stay out of it. The prototype makes one up from the name.
const handleOf = name => name.toLowerCase().replace(/[^a-z0-9._]+/g, "") || "player";

const contactsOf = raw => {
    const slug = handleOf(raw.name || raw.owner);
    return [
        raw.type !== "community" && raw.has_discord && { label: "Discord", value: handleOf(raw.owner) },
        raw.type !== "player" && raw.has_discord_server
            && linkContact(raw.type === "community" ? "Discord invite" : "Discord server", `discord.gg/${slug}`),
        raw.type !== "player" && raw.has_website && linkContact("Website", `${slug}.gg`),
    ].filter(Boolean);
};

// Posts. The sample's rows, with their answers under the keys a draft uses.

// The dump records neither how many a group is nor how many more it wants, so a
// group's numbers are made up from its id.
const slotsOf = raw => {
    const n = Number(raw.id);
    const wanted = 1 + (n % 3);
    return { size: 1 + (n % 4), wantedFrom: wanted, wantedTo: wanted + (n % 2) };
};

const answersOf = raw => ({
    ...Object.fromEntries(Object.entries(raw.fields)
        .map(([key, values]) => [`field:${key}`, gameField(key).ilk === "multi" ? values : values[0]])),
    ...Object.fromEntries(Object.entries(raw.ranges)
        .map(([key, [from, to]]) => [`range:${key}`, { from: from || undefined, to: to || undefined }])),
    ...Object.fromEntries(raw.flags.map(key => [`field:${key}`, true])),
});

const normalize = raw => {
    const updated = new Date(raw.updated);
    const expired = updated.getTime() + lifetime(raw.type) <= NOW.getTime();
    const contacts = contactsOf(raw);
    return {
        id: raw.id,
        href: postPageHref(GAME.handle, raw.id),
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
        location: raw.country || undefined,
        region: REGION_OF[raw.country],
        regions: raw.regions,
        languages: raw.languages,
        hours: raw.online_from && raw.online_to ? inViewerTime(raw.online_from, raw.online_to, raw.timezone) : null,
        mic: raw.microphone,
        answers: answersOf(raw),
        text: raw.text.trim(),
        reach: raw.reach,
        contacts,
        contact: contactButtonOf(raw.reach, contacts),
        slots: raw.type === "group" ? slotsOf(raw) : undefined,
    };
};

// Posts from drafts: what the post screen writes, and what the prototype's
// accounts have published.

// The game account a post offers is the one its trackers read, else the first
// the game takes that the account page knows (fields.js).
const EXTRAS = (() => {
    const kind = (GAME.trackers[0] || {}).contact || GAME.contacts.find(k => GAME_ACCOUNTS[k]) || "steam";
    return {
        account: GAME_ACCOUNTS[kind],
        trackers: GAME.trackers.filter(t => t.contact === kind),
    };
})();

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
const draftPost = (type, d, owner, updated, { id, own = false, inTheirTime = false } = {}) => {
    const named = (d.name || "").trim();
    const name = type === "player" ? owner || "You"
        : named || (owner ? undefined : type === "group" ? "Your group" : "Your community");
    const hours = d.hours && d.hours.from && d.hours.to
        ? (inTheirTime ? inViewerTime(d.hours.from, d.hours.to, d.timezone) : { from: toMinutes(d.hours.from), to: toMinutes(d.hours.to) })
        : null;
    const at = updated ? new Date(updated) : NOW;
    const reach = type === "community" ? d.join || "message" : d.reach || "message";
    const contacts = draftContacts(type, d);
    return {
        id: id || "draft",
        // A draft has no page of its own: only a published post's name links to one.
        href: id && postPageHref(GAME.handle, id),
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
        region: REGION_OF[d.location],
        regions: d.regions || [],
        languages: d.languages || [],
        hours,
        mic: !!d.mic,
        answers: Object.fromEntries(fieldsFor(type)
            .map(f => [answerKey(f, type), d[answerKey(f, type)]])
            .filter(([, value]) => has(value))),
        text: (d.text || "").trim(),
        reach,
        contacts,
        contact: contactButtonOf(reach, contacts),
        slots: type === "group"
            ? { size: Number(d.size) || undefined, wantedFrom: Number(d.wantedFrom) || undefined, wantedTo: Number(d.wantedTo) || undefined }
            : undefined,
        trackers: type === "player" && d.gameAccount
            ? EXTRAS.trackers.map(t => ({ title: t.title, url: t.template + encodeURIComponent(d.gameAccount) }))
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
    return knownPeople().flatMap(person => person.posts
        .filter(p => p.game === GAME.handle && p.draft)
        .map(p => draftPost(p.type, withPerson(p.type, p.draft, person), person.nickname, p.updated, {
            id: postId(person, p),
            own: !!account && person.id === account.id,
            inTheirTime: true,
        })));
};

// Every post tells its owner when a post that fits it appears (brief 8). A
// published post is compared with the other players' posts in this game the way
// the feed compares it with their description, and the owners of the ones it
// fits are notified. A post that has expired isn't told until it is renewed,
// and a block stops it both ways (brief 10).
const notifyOwnersFitBy = (post, type, owner) => {
    knownPeople()
        .filter(person => person.nickname !== owner && !blockedBetween(person.nickname, owner))
        .forEach(person => person.posts
            .filter(p => p.game === GAME.handle && p.draft && !expiredPost(p))
            .forEach(p => {
                const described = describedBy(p.type, withPerson(p.type, p.draft, person));
                const m = compare(post, p.type, described);
                if (!m.compared || m.misses) return;
                addNotification({
                    id: `n-${postId(person, p)}-${post.id}`,
                    for: person.nickname,
                    post: { id: postId(person, p), game: p.game, type: p.type, name: storedPostName(person, p) },
                    kind: "fits",
                    about: { id: post.id, game: GAME.handle, type, name: post.name || `${owner}'s ${type}` },
                    at: NOW.toISOString(),
                    read: false,
                });
            }));
};

const hourOptions = Array.from({ length: 24 }, (_, h) =>
    ({ value: `${String(h).padStart(2, "0")}:00`, label: clock(h * 60) }));

// The bar carries only the fields matching compares for the type (7.1): the
// game's fields that lead the card and the account's facts first, and the rest
// under More.
const gameDescriptionField = (f, type) => {
    const key = answerKey(f, type);
    const more = !f.onCard;
    if (f.ilk === "boolean") {
        return { key, label: flagText(f, type), kind: "toggle", toggleLabel: type === "player" ? `I can be ${article(f.label)} ${f.label.toLowerCase()}` : flagText(f, type), more };
    }
    if (f.ordered) {
        return type === "player"
            ? { key, label: f.label, kind: "select", options: optionsOf(f), placeholder: `Choose your ${f.label.toLowerCase()}`, more }
            : { key, label: `${f.label} range`, kind: "rankRange", options: optionsOf(f), more };
    }
    return { key, label: f.label, kind: f.ilk, options: optionsOf(f), all: f.slotted && anyText(f), more };
};

const descriptionFields = type => {
    const game = fieldsFor(type).map(f => gameDescriptionField(f, type));
    const languages = { key: "languages", label: "Languages", kind: "multi", options: languageOptions };
    const regions = { key: "regions", label: "Regions", kind: "multi", options: regionOptions };
    const hours = { key: "hours", label: "Usually online", kind: "hours", more: true };
    const mic = { key: "mic", label: "Microphone", kind: "toggle", icon: "mic", more: true,
        toggleLabel: type === "player" ? "I use a microphone" : "Microphone required" };
    const fields = {
        player: [
            { key: "location", label: "Location", kind: "select", options: locationOptions, placeholder: "Choose a country" },
            languages,
            { key: "age", label: "Age", kind: "number" },
        ],
        group: [regions, languages, { key: "ageRange", label: "Ages", kind: "ageRange" }],
        community: [regions, languages, { key: "ageRange", label: "Ages", kind: "ageRange" }],
    };
    return game.filter(f => !f.more).concat(fields[type], game.filter(f => f.more), hours, mic);
};

const shortList = labels => labels.length > 2 ? `${labels.slice(0, 2).join(", ")} +${labels.length - 2}` : labels.join(", ");

const summary = (f, value) => {
    if (isEmpty(value)) return null;
    switch (f.kind) {
        case "single": return f.options.find(o => o.value === value).label;
        case "select": return (f.options.find(o => o.value === value) || { label: value }).label;
        case "multi":
            if (f.key === "languages") return value.map(languageCode).join(", ");
            if (f.key === "regions") {
                return value.length === REGIONS.length ? "Anywhere" : shortList(value.map(r => REGION_SHORT[r]));
            }
            if (f.all && value.length === f.options.length) return f.all;
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
const covers = (a, b) => new Set([...a, ...b]).size > 1;
const within = (x, from, to) => x >= (from ?? -Infinity) && x <= (to ?? Infinity);

// A game field is compared by what the field is, never by which field it is.
// An ordered field is near between two players and inside the range against a
// group; a slotted one is covered between two players and filled against a
// group; a boolean fits on agreement, and only where the viewer said yes, since
// a no is every post's default; anything else fits on a shared option.
const compareGameField = (f, post, type, d, check, m) => {
    const key = `field:${f.key}`;
    const players = type === "player" && post.type === "player";
    if (f.ilk === "boolean") {
        if (d[key]) m[key] = fit(post.answers[key]);
    } else if (f.ordered) {
        const mine = orderedIn(f, d, type);
        const theirs = orderedIn(f, post.answers, post.type);
        check(key, mine !== undefined, theirs !== undefined, () =>
            players ? Math.abs(mine - theirs) <= nearSteps(f)
            : type === "player" ? within(mine, ...theirs)
            : within(theirs, ...mine));
    } else {
        const mine = chosenIn(f, d);
        const theirs = chosenIn(f, post.answers);
        check(key, mine.length, theirs.length, () =>
            f.slotted && players ? covers(mine, theirs) : overlaps(mine, theirs));
    }
};

const compare = (post, type, d) => {
    const m = {};
    const check = (key, viewerGave, postGave, fits) => {
        if (viewerGave) m[key] = postGave ? fit(fits()) : "missing";
    };
    // A field that one of the two types isn't asked counts neither way.
    fieldsFor(type).filter(f => f.appliesTo.includes(post.type))
        .forEach(f => compareGameField(f, post, type, d, check, m));

    const hoursGiven = has(d.hours) && d.hours.from && d.hours.to;
    const viewerHours = () => ({ from: toMinutes(d.hours.from), to: toMinutes(d.hours.to) });
    check("hours", hoursGiven, post.hours, () => hoursOverlap(viewerHours(), post.hours));
    // A microphone is compared only where the viewer gave one: a player says
    // they use one, a group or a community that it wants one.
    if (d.mic) m.mic = fit(post.mic);
    check("languages", has(d.languages), post.languages.length, () => overlaps(d.languages, post.languages));

    // A group and a community ask for the players they want in the same
    // account facts (brief 5.3), so a player compares with them alike.
    if (type === "player") {
        const region = has(d.location) ? REGION_OF[d.location] : undefined;
        if (post.type === "player") {
            check("location", region, post.region, () => region === post.region);
            check("age", has(d.age), post.age, () => Math.abs(Number(d.age) - post.age) <= 3);
        } else {
            check("location", region, post.regions.length, () => post.regions.includes(region));
            check("ages", has(d.age), post.ageFrom || post.ageTo, () => within(Number(d.age), post.ageFrom, post.ageTo));
        }
    } else if (post.type === "player") {
        check("age", has(d.ageRange), post.age, () =>
            within(post.age, Number(d.ageRange.from) || undefined, Number(d.ageRange.to) || undefined));
        check("location", has(d.regions), post.region, () => d.regions.includes(post.region));
    }

    const marks = Object.values(m);
    m.compared = marks.length;
    m.fits = marks.filter(v => v === "fit").length;
    m.misses = marks.length - m.fits;
    return m;
};

// Cards: a post with its marks, in the shape prototype.js renders.

const agesText = (from, to) =>
    from && to ? `Ages ${from}–${to}` : from ? `Ages ${from}+` : `Ages up to ${to}`;

const missingText = (key, post) => isAnswerKey(key)
    ? `${gameField(key.slice(6)).label} not given`
    : ({
        languages: "Languages", hours: "Online hours", age: "Age", ages: "Ages",
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
    const gameFact = (f, shown) => {
        const key = `field:${f.key}`;
        const text = answerText(f, post);
        const named = LADDERS_NAMED && f.ordered ? `${f.label} ${text}` : text;
        if (f.ilk === "boolean" && !text && m[key] === "miss") {
            facts.push({ text: flagText(f, post.type, false), match: "miss" });
        } else {
            slot(key, text && shown(key), { text: named });
        }
    };
    const regions = post.regions.length === REGIONS.length
        ? "Anywhere"
        : post.regions.map(r => REGION_SHORT[r] || r).join(", ");
    const fields = fieldsFor(post.type);

    // A group's or a community's facts say what it is looking for, and a
    // player's what they are; the post's type says which, so nothing in the
    // line marks it (brief 5). The game's fields that lead the card come
    // first, in the game's order.
    fields.filter(f => f.onCard).forEach(f => gameFact(f, () => true));
    if (post.type === "player") slot("location", post.location, { text: post.location });
    else slot("location", post.regions.length, { text: regions });
    slot("languages", post.languages.length, { text: post.languages.map(languageCode).join(", ") });
    if (post.mic) {
        facts.push({ icon: "mic", label: post.type === "player" ? "Microphone" : "Microphone required", match: match("mic") });
    } else if (m.mic === "miss") {
        facts.push({ icon: "mic-off", label: "No microphone", match: "miss" });
    }
    if (post.type !== "player") slot("ages", post.ageFrom || post.ageTo, { text: agesText(post.ageFrom, post.ageTo) });
    // The game's other fields, online hours and a player's age wait behind
    // Details, and join the end of the line only while compared.
    fields.filter(f => !f.onCard).forEach(f => gameFact(f, key => m[key]));
    slot("hours", post.hours && m.hours, { text: post.hours && formatHours(post.hours), tabular: true });
    if (post.type === "player") slot("age", post.age && m.age, { text: `Age ${post.age}` });

    const details = fields.filter(f => !f.onCard && answerText(f, post)).map(f => ({
        label: f.label,
        value: f.ilk === "boolean" ? (post.type === "player" ? "Yes" : "Needed") : answerText(f, post),
    }));
    if (post.hours) {
        const allDay = post.hours.from === post.hours.to;
        details.unshift({ label: "Usually online", value: allDay ? "All day" : formatHours(post.hours) });
    }
    if (post.type === "player" && post.age) details.unshift({ label: "Age", value: String(post.age) });

    const long = post.text.length > (post.type === "community" ? 360 : 170) || post.text.split("\n").length > 2;

    return {
        id: post.id,
        href: post.href,
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
        multi: () => options("checkbox", o => (value || []).includes(o)) + allButton(f),
        select: () => select("value", f.options, value, f.placeholder),
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

const POSTS = GAME.posts.map(normalize).concat(accountPosts());

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
