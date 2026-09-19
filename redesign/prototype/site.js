// What every prototype page shares: the clock, the games, the accounts the
// prototype bar switches between, the conversations and blocks that stand in
// for the server, the header and the prototype bar. Classic script, loaded
// after prototype.js and before any page's own.

// Time. The dump's date stands in for now, so freshness reads as it did that day.

const NOW = new Date("2026-09-12T08:23:58Z");
const DAY = 864e5;

const lifetime = type => (type === "community" ? 90 : 30) * DAY;

const ago = date => {
    const minutes = Math.round((NOW - date) / 60000);
    const unit = (n, name) => `${n} ${name}${n === 1 ? "" : "s"} ago`;
    if (minutes < 1) return "just now";
    if (minutes < 60) return unit(minutes, "minute");
    const hours = Math.round(minutes / 60);
    if (hours < 24) return unit(hours, "hour");
    const days = Math.round(hours / 24);
    if (days < 14) return unit(days, "day");
    if (days < 60) return unit(Math.round(days / 7), "week");
    if (days < 365) return unit(Math.round(days / 30), "month");
    return unit(Math.round(days / 365), "year");
};

// Messages sent in the prototype are stamped a second apart from now, so they
// keep their order and read as just sent.
const CLOCK_KEY = "tt-proto-clock";
const stamp = () => {
    const tick = Number(localStorage.getItem(CLOCK_KEY) || 0) + 1;
    localStorage.setItem(CLOCK_KEY, tick);
    return new Date(NOW.getTime() + tick * 1000).toISOString();
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

const timeOfDay = date => clock(date.getHours() * 60 + date.getMinutes());

// The viewer's own timezone: what a post's hours are read in, and what an
// account without one of its own writes them in (brief 6, step 3).
const VIEWER_TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;

// Month names are in the site's language; the order of day and month follows
// the viewer's locale where it is an English one.
const DATE_LOCALE = navigator.language.startsWith("en") ? navigator.language : "en-GB";

const dayLabel = date => {
    const start = d => new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();
    const days = Math.round((start(NOW) - start(date)) / DAY);
    if (days <= 0) return "Today";
    if (days === 1) return "Yesterday";
    return date.toLocaleDateString(DATE_LOCALE, { day: "numeric", month: "long" });
};

const ageAt = birthday => {
    if (!birthday) return undefined;
    const born = new Date(birthday);
    const months = NOW.getUTCMonth() - born.getUTCMonth();
    const beforeBirthday = months < 0 || (months === 0 && NOW.getUTCDate() < born.getUTCDate());
    const age = NOW.getUTCFullYear() - born.getUTCFullYear() - (beforeBirthday ? 1 : 0);
    return age > 0 && age < 120 ? age : undefined;
};

const isEmpty = value =>
    value === undefined || value === null || value === "" || value === false
    || (Array.isArray(value) && value.length === 0)
    || (typeof value === "object" && !Array.isArray(value) && Object.values(value).every(isEmpty));

const has = value => !isEmpty(value);

const readJson = (key, fallback) => {
    try {
        return JSON.parse(localStorage.getItem(key)) ?? fallback;
    } catch {
        return fallback;
    }
};

// Post types. The type chooser uses the player's own words, each with a short
// example (brief 6, step 1); the home page and the first step of posting show it.

const TYPE_CARDS = [
    { type: "player", icon: "user", choice: "I'm a player looking for a group", example: "Groups, communities and other players find you" },
    { type: "group", icon: "users", choice: "We're a group looking for players", example: "“Three of us play most nights, need a fifth”" },
    { type: "community", icon: "castle", choice: "We're a community looking for members", example: "“Our server runs weekly events, all welcome”" },
];

// href(type) is where each card leads; note(type) is an optional line under
// the example.
const typeCardsHtml = (href, note = () => "") => `<div class="type-cards">${TYPE_CARDS.map(t => `<a class="type-card" href="${href(t.type)}">
    ${icon(t.icon)}
    <span class="type-card-text">
        <span class="type-card-title">${t.choice}</span>
        <span class="type-card-example">${t.example}</span>
        ${note(t.type) ? `<span class="type-card-mine">${note(t.type)}</span>` : ""}
    </span>
    ${icon("chevron-right")}
</a>`).join("")}</div>`;

// What a post says about its owner, as the feed's description takes it: the
// fields matching compares for the type, and a player's age from their
// birthday (brief 7.1).
const DESCRIBED = {
    player: ["rank", "roles", "platforms", "location", "languages", "lookingFor", "hours", "mic"],
    group: ["roles", "rankRange", "platforms", "regions", "languages", "ageRange", "lookingFor", "hours", "mic"],
    community: ["regions", "languages", "platforms", "lookingFor"],
};

const describedBy = (type, d) => {
    const described = Object.fromEntries(DESCRIBED[type].filter(k => !isEmpty(d[k])).map(k => [k, d[k]]));
    if (type === "player" && ageAt(d.birthday)) described.age = String(ageAt(d.birthday));
    return described;
};

// See what fits and the Matches screen open a game's feed with the description
// taken from a post (brief 11.2).
const describeFeed = (game, type, described) => {
    const key = `tt-description-${game}`;
    const stored = readJson(key, null) || { type: "player", player: {}, group: {}, community: {} };
    stored.type = type;
    stored[type] = described;
    localStorage.setItem(key, JSON.stringify(stored));
};

// Games.

const GAMES = [
    { handle: "apex", title: "Apex Legends" },
    { handle: "csgo", title: "Counter Strike: Global Offensive" },
    { handle: "dota2", title: "Dota 2" },
    { handle: "hots", title: "Heroes of the Storm" },
    { handle: "lol", title: "League of Legends" },
    { handle: "overwatch", title: "Overwatch" },
    { handle: "r6s", title: "Rainbow Six: Siege" },
    { handle: "splitgate", title: "Splitgate" },
    { handle: "tf2", title: "Team Fortress 2" },
    { handle: "valheim", title: "Valheim" },
    { handle: "valorant", title: "Valorant" },
];
const gameTitle = handle => (GAMES.find(g => g.handle === handle) || { title: handle }).title;
const coverOf = handle => `../../src/TeamTavern/Client/Static/Images/Games/${handle}.webp`;

// The cover grid: the game picker in the header (brief 11.4), on the home page
// (11.2) and in the second step of posting (6). It carries no captions: each
// cover's logo names its game, and the title is the image's alt text (14.3).
// href says where a cover leads, and mark puts a note on it, such as the
// player's own post.
const coverGridHtml = (games, { href = g => feedHref(g.handle), mark = () => "" } = {}) =>
    `<div class="cover-grid">${games.map(g => `<a class="cover" href="${href(g)}">
        <img src="${coverOf(g.handle)}" alt="${escapeHtml(g.title)}">
        ${mark(g) ? `<span class="cover-mark">${escapeHtml(mark(g))}</span>` : ""}
    </a>`).join("")}</div>`;

// Where a game's feed and a post's own page live. A post's page is what is
// shared, crawled and linked to from a card's name, the home page, a match
// email and a notification (brief 11.1).
const feedHref = game => `feed.html?game=${encodeURIComponent(game)}`;
const postPageHref = (game, id) =>
    `post-page.html?game=${encodeURIComponent(game)}&id=${encodeURIComponent(id)}`;

// Accounts. The prototype bar switches between these; registering makes a new one.

const NIGHT_OWLS = {
    name: "Night Owls",
    members: 3,
    total: 5,
    roles: ["lurker", "supporter"],
    rankRange: { from: "platinum", to: "diamond" },
    regions: ["Europe"],
    languages: ["English", "Croatian"],
    mic: true,
    ageRange: { from: "18" },
    lookingFor: ["competitive"],
    text: "Three friends who play most nights, we want to stop solo queuing for the last two spots. No tilt, comms on, we review our losses on Sundays.",
    reach: "message",
    hours: { from: "21:00", to: "01:00" },
};

// Kestrel's Valheim post, which is about to expire.
const VALHEIM_DUO = {
    lookingFor: ["pve", "building"],
    "field:server-characters": ["new-characters"],
    "field:server-type": ["vanilla"],
    mic: true,
    returning: true,
    text: "Starting over with a friend after a long break. We'd like a small vanilla server with a few people who build and go after the bosses together in the evenings.",
    reach: "either",
    hours: { from: "20:00", to: "23:00" },
};

// A post's card as its owner last published it. The home page shows posts from
// every game, and game.js holds one game at a time, so publishing keeps a copy.
const NIGHT_OWLS_CARD = {
    slots: { members: 3, total: 5 },
    facts: [
        { text: "Platinum – Diamond" },
        { text: "Needs Lurker, Supporter" },
        { text: "EU" },
        { text: "EN, HR" },
        { icon: "mic", label: "Microphone required" },
        { text: "Ages 18+" },
        { text: "Competitive" },
    ],
};
const VALHEIM_DUO_CARD = {
    facts: [{ text: "Croatia" }, { text: "HR, EN" }, { icon: "mic", label: "Microphone" }, { text: "PvE, Building" }, { text: "Returning player" }],
};
const KESTREL_DOTA_CARD = {
    facts: [{ text: "Legend 2" }, { text: "Soft support, Hard support" }, { text: "Croatia" }, { text: "HR, EN" }],
};

const PRESETS = {
    kestrel: {
        id: "kestrel",
        nickname: "Kestrel",
        email: "kestrel@example.com",
        location: "Croatia",
        languages: ["Croatian", "English"],
        birthday: "1998-04-12",
        timezone: "Europe/Zagreb",
        discord: "kestrel",
        accounts: { riot: "Kestrel#EUW" },
        posts: [
            { game: "valorant", type: "group", updated: "2026-09-06T19:12:00Z", draft: NIGHT_OWLS, card: NIGHT_OWLS_CARD },
            { game: "valheim", type: "player", updated: "2026-08-16T10:00:00Z", draft: VALHEIM_DUO, card: VALHEIM_DUO_CARD },
            { game: "dota2", type: "player", updated: "2026-07-20T17:40:00Z", card: KESTREL_DOTA_CARD },
        ],
    },
    vex: {
        id: "vex",
        nickname: "Vex",
        email: "vex@example.com",
        discord: "vex.omen",
        accounts: {},
        posts: [],
    },
};

// A post is known by its owner, game and type: a player has at most one of each.
const postId = (person, post) => `${person.id}-${post.game}-${post.type}`;

// What a post of a player's is called, as its card's heading reads it: a group
// or community may go unnamed, and a player post takes the nickname.
const storedPostName = (person, stored) => (stored.draft && stored.draft.name)
    || (stored.type === "player" ? person.nickname : `${person.nickname}'s ${stored.type}`);

// What each account has published outlives the bar switching to someone else,
// the way their conversations do: the post a viewer publishes stays in its
// game's feed, keeps its page, and keeps whatever it told other players.
const PUBLISHED_KEY = "tt-proto-published";
const publishedPosts = () => readJson(PUBLISHED_KEY, {});
const postsOf = person => publishedPosts()[person.id] || person.posts;

// The facts an account holds outlive the switch the same way, so a nickname,
// location or contact changed on the account page (brief 11.5) shows on that
// player's posts whoever is viewing.
const FACTS_KEY = "tt-proto-facts";
const storedFacts = () => readJson(FACTS_KEY, {});
const factsOf = person => ({ ...person, ...(storedFacts()[person.id] || {}) });

const ACCOUNT_KEY = "tt-proto-account";
let account = readJson(ACCOUNT_KEY, null);
const saveAccount = () => {
    localStorage.setItem(ACCOUNT_KEY, JSON.stringify(account));
    if (!account) return;
    const { posts, ...facts } = account;
    localStorage.setItem(PUBLISHED_KEY, JSON.stringify({ ...publishedPosts(), [account.id]: posts }));
    localStorage.setItem(FACTS_KEY, JSON.stringify({ ...storedFacts(), [account.id]: facts }));
};
const useAccount = id => {
    account = id === "out" ? null : factsOf(structuredClone(PRESETS[id]));
    if (account) account.posts = postsOf(account);
    saveAccount();
};

// Everyone the prototype knows: its accounts as the stores have them, and
// whoever is viewing, who may be neither of them.
const knownPeople = () => {
    const people = Object.values(PRESETS)
        .map(p => (account && account.id === p.id ? account : { ...factsOf(p), posts: postsOf(p) }));
    if (account && !PRESETS[account.id]) people.push(account);
    return people.filter(person => person.nickname);
};

// Switching accounts from the prototype bar starts every draft over, as signing
// out would.
const switchAccount = id => {
    Object.keys(localStorage).filter(k => k.startsWith("tt-draft-")).forEach(k => localStorage.removeItem(k));
    useAccount(id);
};

// ?as=out|kestrel|vex does what the bar does, from a URL.
(() => {
    const params = new URLSearchParams(location.search);
    if (!params.get("as")) return;
    switchAccount(params.get("as"));
    params.delete("as");
    history.replaceState(null, "", `${location.pathname.split("/").pop()}${params.size ? `?${params}` : ""}`);
})();

// Which page the tab was on before this one. A post's page offers Back to the
// feed only when the feed really is behind it, since Back is what keeps the
// feed's loaded batches (brief 11.1).
const PREVIOUS_KEY = "tt-proto-previous";
const previousPage = sessionStorage.getItem(PREVIOUS_KEY) || "";
sessionStorage.setItem(PREVIOUS_KEY, `${location.pathname.split("/").pop()}${location.search}`);

// Signing up with Discord makes an account without a nickname; the register
// step asks for one.
const signedIn = () => !!(account && account.nickname);

// Where the site's emails to a player stand (brief 6, step 4; 11.5). An address
// is confirmed by its link, or by Discord verifying it; until then the link is
// the only email it gets. A Discord account may come with no address at all.
const emailState = person => !person.email ? "none" : person.emailConfirmed === false ? "unconfirmed" : "confirmed";
const me = () => (signedIn() ? account.nickname : null);

// Contact reveals: how often a post's contact panel showed its contacts or
// join links, which the home page counts for the owner (brief 5.6, 11.2). The
// counts the prototype starts with are made up; opening a panel adds to them.

const REVEALS_KEY = "tt-proto-reveals";
const STARTING_REVEALS = { "kestrel-valorant-group": 14, "kestrel-valheim-player": 2, "kestrel-dota2-player": 9 };
const revealsOf = id => (STARTING_REVEALS[id] || 0) + (readJson(REVEALS_KEY, {})[id] || 0);
const reveal = id => {
    const counts = readJson(REVEALS_KEY, {});
    counts[id] = (counts[id] || 0) + 1;
    localStorage.setItem(REVEALS_KEY, JSON.stringify(counts));
};

// Blocks and reports. A block hides two players from each other, both ways
// (brief 10). Players are known by nickname here.

const BLOCKS_KEY = "tt-proto-blocks";
const REPORTS_KEY = "tt-proto-reports";
let blocks = readJson(BLOCKS_KEY, []);
const saveBlocks = () => localStorage.setItem(BLOCKS_KEY, JSON.stringify(blocks));

const blockedBetween = (a, b) => blocks.some(x => (x.by === a && x.who === b) || (x.by === b && x.who === a));
const hiddenFromViewer = nickname => signedIn() && blockedBetween(me(), nickname);
const block = who => {
    blocks.push({ by: me(), who });
    saveBlocks();
};
const unblock = who => {
    blocks = blocks.filter(x => !(x.by === me() && x.who === who));
    saveBlocks();
};
const report = entry => localStorage.setItem(REPORTS_KEY, JSON.stringify(readJson(REPORTS_KEY, []).concat({ by: me(), ...entry })));

// Who the viewer has blocked, which the account page lists and unblocks from
// (brief 11.5).
const blockedByViewer = () => blocks.filter(x => x.by === me()).map(x => x.who);

// The email switches on the account page, one per kind of email (brief 11.5).
// Nothing here sends email, so they keep their state and say what they would do.
const EMAILS_KEY = "tt-proto-emails";
const emailSettings = () => ({ matches: true, messages: true, renewals: true, ...readJson(EMAILS_KEY, {}) });
const setEmailSetting = (kind, on) =>
    localStorage.setItem(EMAILS_KEY, JSON.stringify({ ...emailSettings(), [kind]: on }));

// Conversations. Each is one player and one post: { id, post, starter,
// messages, read }, where post is what the conversation keeps of the post,
// starter is the player who wrote first, and read counts the messages each side
// has seen. Seeded from conversations.js.

const CONVERSATIONS_KEY = "tt-proto-conversations";
let conversationStore = null;
const allConversations = () => conversationStore ??= readJson(CONVERSATIONS_KEY, null)
    || structuredClone(typeof CONVERSATION_FIXTURES === "undefined" ? [] : CONVERSATION_FIXTURES);
const saveConversations = () => localStorage.setItem(CONVERSATIONS_KEY, JSON.stringify(allConversations()));

const otherIn = (c, who) => (c.post.owner === who ? c.starter.nickname : c.post.owner);

const visibleConversations = () => !signedIn() ? [] : allConversations()
    .filter(c => (c.post.owner === me() || c.starter.nickname === me()) && !hiddenFromViewer(otherIn(c, me())));

const unreadIn = (c, who) => c.messages.slice(c.read[who] || 0).filter(m => m.from !== who).length;
const unreadConversations = () => visibleConversations().filter(c => unreadIn(c, me())).length;
const conversationById = id => visibleConversations().find(c => c.id === id);
const conversationAbout = id => visibleConversations().find(c => c.post.id === id && c.starter.nickname === me());
const conversationsOfPost = id => allConversations().filter(c => c.post.id === id);

// Deleting a post deletes its conversations, for both sides (brief 10).
const deleteConversationsOf = id => {
    conversationStore = allConversations().filter(c => c.post.id !== id);
    saveConversations();
};

const markRead = c => {
    c.read[me()] = c.messages.length;
    saveConversations();
};

// Sending says whether the recipient is emailed: only when the conversation had
// nothing unread for them (brief 10).
const sendMessage = (c, from, text) => {
    const emailed = unreadIn(c, otherIn(c, from)) === 0;
    c.messages.push({ from, text, at: stamp() });
    c.read[from] = c.messages.length;
    saveConversations();
    return emailed;
};

// What the owner of a post is told about it, on the home page and on the
// post's own page (brief 11.2): how long it stays active, the conversations it
// produced and how often its contacts were shown. A stored post is the account's
// { game, type, updated, draft, card }.

const expiresAt = post => new Date(post.updated).getTime() + lifetime(post.type);
const expiredPost = post => expiresAt(post) <= NOW.getTime();
const daysLeft = post => Math.round((expiresAt(post) - NOW.getTime()) / DAY);

// The conversations link opens the inbox on the post's first conversation in
// the inbox's order, latest first, that has unread messages, else on its first.
const ownPostStats = stored => {
    const id = postId(account, stored);
    const conversations = visibleConversations()
        .filter(c => c.post.id === id)
        .sort((a, b) => new Date(b.messages.at(-1).at) - new Date(a.messages.at(-1).at));
    const unread = conversations.filter(c => unreadIn(c, me()));
    const opened = unread[0] || conversations[0];
    return {
        days: daysLeft(stored),
        expired: expiredPost(stored),
        expiredAgo: ago(new Date(expiresAt(stored))),
        conversations: conversations.length,
        unread: unread.length,
        conversationsHref: opened ? `messages.html?c=${encodeURIComponent(opened.id)}` : "messages.html",
        reveals: revealsOf(id),
    };
};

// Notifications (brief 8, 11.3). Every post tells its owner when a post that
// fits it appears, and before it expires. One is { id, for, post, kind, about,
// at, read }: post is the owner's own post it is grouped under, kind is "fits"
// or "expiry", about is the post that fits, and at is when it was published.
// Seeded from notifications.js; publishing a post adds to it (game.js).

const NOTIFICATIONS_KEY = "tt-proto-notifications";
// The list scrolls rather than paging, and there is no page to send it to, so
// the store keeps only the newest of each player's; older ones fall off.
const NOTIFICATION_LIMIT = 50;

let notificationStore = null;
const allNotifications = () => notificationStore ??= readJson(NOTIFICATIONS_KEY, null)
    || structuredClone(typeof NOTIFICATION_FIXTURES === "undefined" ? [] : NOTIFICATION_FIXTURES);
const saveNotifications = () => localStorage.setItem(NOTIFICATIONS_KEY, JSON.stringify(allNotifications()));

const addNotification = entry => {
    if (allNotifications().some(n => n.id === entry.id)) return;
    const all = allNotifications().concat(entry);
    const theirs = all.filter(n => n.for === entry.for).sort((a, b) => new Date(b.at) - new Date(a.at));
    const kept = new Set(theirs.slice(0, NOTIFICATION_LIMIT).map(n => n.id));
    notificationStore = all.filter(n => n.for !== entry.for || kept.has(n.id));
    saveNotifications();
};

// A notification is grouped under the post it is about, so a post the player
// deleted takes its notifications with it, as it takes its conversations
// (brief 10). An expiry notification says what the post's state is now rather
// than recording a moment, so renewing the post takes it away.
const storedPostOf = n => signedIn() && account.posts.find(p => postId(account, p) === n.post.id);

const visibleNotifications = () => !signedIn() ? [] : allNotifications()
    .filter(n => n.for === me())
    .map(n => ({ ...n, stored: storedPostOf(n) }))
    .filter(n => n.stored && (n.kind !== "expiry" || expiredPost(n.stored) || daysLeft(n.stored) <= EXPIRING_DAYS))
    .sort((a, b) => new Date(b.at) - new Date(a.at));

const unreadNotifications = () => visibleNotifications().filter(n => !n.read).length;

const readNotifications = ids => {
    const wanted = new Set(ids);
    allNotifications().forEach(n => { if (wanted.has(n.id)) n.read = true; });
    saveNotifications();
};

// Each of the player's own posts holds the notifications about it, and the post
// with the newest one is at the top: what just happened leads. Inside a post,
// its own expiry comes first, since it is the row with something to do, and the
// posts that fit follow, newest first.
const notificationGroups = () => {
    const groups = [];
    visibleNotifications().forEach(n => {
        const group = groups.find(g => g.post.id === n.post.id);
        if (group) group.rows.push(n);
        else groups.push({ post: n.post, rows: [n] });
    });
    groups.forEach(g => g.rows.sort((a, b) => (a.kind === "expiry" ? 0 : 1) - (b.kind === "expiry" ? 0 : 1)));
    return groups;
};

// The account itself (brief 11.5). A player is known here by their nickname,
// so changing one rewrites the stores that name them; deleting an account
// takes everything tied to it with it (brief 10).

const renamePerson = (from, to) => {
    if (!from || from === to) return;
    allConversations().forEach(c => {
        if (c.post.owner === from) c.post.owner = to;
        // A player post is named after its owner, where a group or community
        // carries a name of its own.
        if (c.post.type === "player" && c.post.name === from) c.post.name = to;
        if (c.starter.nickname === from) c.starter.nickname = to;
        if (c.starter.post && c.starter.post.type === "player" && c.starter.post.name === from) c.starter.post.name = to;
        c.messages.forEach(m => { if (m.from === from) m.from = to; });
        if (c.read[from] !== undefined) {
            c.read[to] = c.read[from];
            delete c.read[from];
        }
    });
    saveConversations();
    allNotifications().forEach(n => {
        if (n.for === from) n.for = to;
        if (n.post.type === "player" && n.post.name === from) n.post.name = to;
        if (n.about && n.about.type === "player" && n.about.name === from) n.about.name = to;
    });
    saveNotifications();
    blocks.forEach(x => {
        if (x.by === from) x.by = to;
        if (x.who === from) x.who = to;
    });
    saveBlocks();
};

// Every conversation the viewer is in, on their own posts and on other
// players', which deleting the account deletes for both sides.
const conversationsOfViewer = () =>
    allConversations().filter(c => c.post.owner === me() || c.starter.nickname === me());

// What the account leaves behind is other players' notifications about its
// posts: such a row still opens the post's page, which says the post is gone
// (brief 11.1, 11.3).
const deleteViewerAccount = () => {
    const who = me();
    const id = account.id;
    conversationStore = allConversations().filter(c => !(c.post.owner === who || c.starter.nickname === who));
    saveConversations();
    notificationStore = allNotifications().filter(n => n.for !== who);
    saveNotifications();
    blocks = blocks.filter(x => x.by !== who && x.who !== who);
    saveBlocks();
    const { [id]: gone, ...facts } = storedFacts();
    localStorage.setItem(FACTS_KEY, JSON.stringify(facts));
    localStorage.setItem(PUBLISHED_KEY, JSON.stringify({ ...publishedPosts(), [id]: [] }));
    localStorage.removeItem(EMAILS_KEY);
    useAccount("out");
};

// Toasts: a line at the bottom of the screen, with an optional action such as
// Undo. A stand-in toast is the prototype's, saying what the real site would
// do offstage, such as send an email.

const toastActions = {};
let toastCount = 0;

const toast = (text, { action, onAction, standIn } = {}) => {
    let root = document.getElementById("toasts");
    if (!root) {
        root = document.createElement("div");
        root.id = "toasts";
        root.className = "toasts";
        root.setAttribute("role", "status");
        document.body.append(root);
    }
    const id = `toast-${++toastCount}`;
    if (onAction) toastActions[id] = onAction;
    root.insertAdjacentHTML("beforeend", `<div class="toast${standIn ? " stand-in" : ""}" id="${id}">
        ${standIn ? `<span class="stand-in-label">Stand-in</span>` : ""}
        <span class="toast-text">${escapeHtml(text)}</span>
        ${action ? `<button class="button button-text button-small" type="button" data-toast="${id}">${escapeHtml(action)}</button>` : ""}
    </div>`);
    setTimeout(() => {
        document.getElementById(id)?.remove();
        delete toastActions[id];
    }, action ? 8000 : 5000);
};

// Toasts for the page after this one: deleting an account lands on the home
// page with something to say about it, and signing up on wherever it returns to.
const TOAST_KEY = "tt-proto-toast";
const toastOnNextPage = (text, { standIn } = {}) => sessionStorage.setItem(TOAST_KEY,
    JSON.stringify(waitingToasts().concat({ text, standIn })));
const waitingToasts = () => {
    try {
        return JSON.parse(sessionStorage.getItem(TOAST_KEY)) || [];
    } catch {
        return [];
    }
};

(() => {
    const waiting = waitingToasts();
    sessionStorage.removeItem(TOAST_KEY);
    waiting.forEach(t => toast(t.text, { standIn: t.standIn }));
})();

// The header (brief 11.4). Pages may define pageHeaderOptions() returning
// { newPostHref, signInHref, signUpHref, current }.

// The site's one sign-up screen is the post flow's register step. Reached from
// anywhere else, it returns the player to where they were.
const signUpHref = (mode, extra = {}) => {
    const params = new URLSearchParams({ step: "register", ...(mode ? { mode } : {}), next: location.href.split("/").pop(), ...extra });
    return `post.html?${params}`;
};

// One header menu is open at a time: Games, notifications, the account menu,
// or, on a phone signed out, the menu holding Sign in and Sign up. The header
// repaints with it open, so its state lives here rather than in the DOM.
let headerMenu = null;

const phoneWidth = matchMedia("(max-width: 639px)");

// Games: the cover grid, each cover opening that game's feed. For a signed-in
// player, a game they have a post in carries a mark (brief 11.4).
const postsInGame = handle => signedIn() ? account.posts.filter(p => p.game === handle).length : 0;

const gamesMenuHtml = () => coverGridHtml(GAMES, {
    mark: g => !postsInGame(g.handle) ? "" : postsInGame(g.handle) === 1 ? "Your post" : "Your posts",
});

// The notification list (brief 11.3): the player's own posts, each holding the
// notifications about it. A post that fits opens its own page, and a post of
// yours about to expire opens your posts, where it is renewed.

const notificationHref = n => n.kind === "expiry" ? "home.html" : postPageHref(n.about.game, n.about.id);

// An expiry notification reads the post's state in the same words the home page
// gives its owner; one about a post that fits names it and its type.
const notificationLines = n => {
    if (n.kind !== "expiry") {
        return {
            title: `${n.about.name} fits`,
            meta: `${TYPES[n.about.type].label} · ${ago(new Date(n.at))}`,
        };
    }
    // The row says what the state is and where to act on it; the page it opens
    // says the rest.
    const state = ownPostStateText(ownPostStats(n.stored));
    return { icon: state.icon, title: state.text, meta: "Renew it from your posts." };
};

const notificationRowHtml = n => {
    const lines = notificationLines(n);
    return `<a class="notification${n.read ? "" : " notification-unread"}" href="${notificationHref(n)}" data-notification="${escapeHtml(n.id)}">
        <span class="notification-mark">${n.read ? "" : `<span class="unread-dot"></span><span class="visually-hidden">Unread</span>`}</span>
        <span class="notification-main">
            <span class="notification-title">${lines.icon ? icon(lines.icon) : ""}${escapeHtml(lines.title)}</span>
            <span class="notification-meta">${escapeHtml(lines.meta)}</span>
        </span>
    </a>`;
};

// Mark all read sits in the heading row on a desktop, and above the list on a
// phone, whose heading row is the overlay's. It shows only while there is
// something to read.
const markAllReadHtml = () => !unreadNotifications() ? ""
    : `<button class="button button-text button-small" type="button" data-header="read-all">Mark all read</button>`;

const notificationsMenuHtml = () => {
    const groups = notificationGroups();
    if (!groups.length) {
        return `<div class="notifications-empty">
            <p>No notifications yet. Every post tells you when someone new fits it, and before it expires.</p>
            <a class="button button-outline button-small" href="post.html">${icon("plus")}New post</a>
        </div>`;
    }
    const markAllRead = phoneWidth.matches ? markAllReadHtml() : "";
    return `${markAllRead ? `<div class="notifications-actions">${markAllRead}</div>` : ""}
    <div class="notifications">${groups.map(g => `<section class="notification-group">
        <h3 class="notification-heading">${escapeHtml(g.post.name)} · ${escapeHtml(gameTitle(g.post.game))} ${escapeHtml(g.post.type)}</h3>
        ${g.rows.map(notificationRowHtml).join("")}
    </section>`).join("")}</div>`;
};

// The account menu (brief 11.4).
const accountMenuHtml = () => `
    <a class="menu-item" role="menuitem" href="home.html">Your posts</a>
    <a class="menu-item" role="menuitem" href="account.html">Account</a>
    <div class="menu-divider" role="separator"></div>
    <button class="menu-item" type="button" role="menuitem" data-header="sign-out">Sign out</button>`;

// Signed out on a phone there is no room for both links, so a menu holds them.
const signedOutMenuHtml = options => `
    <a class="menu-item" role="menuitem" href="${options.signInHref || signUpHref("signin")}">Sign in</a>
    <a class="menu-item" role="menuitem" href="${options.signUpHref || signUpHref()}">Sign up</a>`;

// Games and notifications are panels; the account menu and the signed-out menu
// are lists of items, which take the same menu as a conversation's ⋯.
const headerMenuIsList = () => headerMenu === "account" || headerMenu === "menu";

const headerMenuLabel = () =>
    headerMenu === "games" ? "Games"
    : headerMenu === "notifications" ? "Notifications"
    : headerMenu === "account" ? account.nickname
    : "Menu";

const headerMenuBody = options =>
    headerMenu === "games" ? gamesMenuHtml()
    : headerMenu === "notifications" ? notificationsMenuHtml()
    : headerMenu === "account" ? accountMenuHtml()
    : signedOutMenuHtml(options);

// On a desktop a menu is a dropdown under the button that opened it.
const headerDropdownHtml = options => {
    if (!headerMenu || phoneWidth.matches) return "";
    if (headerMenuIsList()) {
        return `<div class="menu header-menu" role="menu" aria-label="${escapeHtml(headerMenuLabel())}">
            <p class="menu-label">${escapeHtml(headerMenuLabel())}</p>
            ${headerMenuBody(options)}
        </div>`;
    }
    return `<div class="header-dropdown header-dropdown-${headerMenu} header-menu" role="dialog" aria-label="${headerMenuLabel()}">
        ${headerMenu === "notifications" ? `<div class="header-menu-heading"><h2>Notifications</h2>${markAllReadHtml()}</div>` : ""}
        ${headerMenuBody(options)}
    </div>`;
};

// On a phone Games and notifications open full-screen, like the feed's
// description bar, and the account menu as a sheet from the bottom: it holds
// four short rows, and a whole screen for them would read as a page.
const headerOverlayHtml = options => {
    if (!headerMenu || !phoneWidth.matches) return "";
    const sheet = headerMenuIsList();
    const body = headerMenuBody(options);
    return `${sheet ? `<div class="backdrop" data-header="close"></div>` : ""}
    <div class="overlay header-menu${sheet ? " overlay-bottom" : ""}" role="dialog" aria-modal="true" aria-labelledby="header-menu-title">
        <div class="overlay-header">
            <h2 id="header-menu-title" style="font-size: 16px">${escapeHtml(headerMenuLabel())}</h2>
            <button class="icon-button" type="button" data-header="close" aria-label="Close">${icon("x")}</button>
        </div>
        <div class="overlay-body">${sheet ? `<div class="sheet-menu">${body}</div>` : body}</div>
    </div>`;
};

const headerHtml = () => {
    const options = { newPostHref: "post.html", ...(typeof pageHeaderOptions === "function" ? pageHeaderOptions() : {}) };
    const count = n => n ? `<span class="badge" aria-hidden="true">${n}</span>` : "";
    const messages = unreadConversations();
    const notifications = unreadNotifications();
    const open = name => headerMenu === name;
    // The dropdown is rendered inside the wrap of the button it belongs to, so
    // it hangs from it; a phone's overlay hangs from nothing and sits after the
    // header, outside its stacking context.
    const dropdown = name => open(name) ? headerDropdownHtml(options) : "";
    const right = signedIn()
        ? `<a class="icon-button header-count" href="messages.html" aria-label="Messages${messages ? `, ${messages} unread` : ""}"${options.current === "messages" ? ` aria-current="page"` : ""}>${icon("mail")}${count(messages)}</a>
           <div class="header-wrap">
               <button class="icon-button header-count" type="button" data-header="notifications" aria-haspopup="dialog" aria-expanded="${open("notifications")}" aria-label="Notifications${notifications ? `, ${notifications} new` : ""}">${icon("bell")}${count(notifications)}</button>
               ${dropdown("notifications")}
           </div>
           <div class="header-wrap">
               <button class="avatar" type="button" data-header="account" aria-haspopup="menu" aria-expanded="${open("account")}" aria-label="Account menu">${escapeHtml(account.nickname[0].toUpperCase())}</button>
               ${dropdown("account")}
           </div>`
        : `<a class="button button-text button-small hide-phone" href="${options.signInHref || signUpHref("signin")}">Sign in</a>
           <a class="button button-text button-small hide-phone" href="${options.signUpHref || signUpHref()}">Sign up</a>
           <div class="header-wrap show-phone">
               <button class="icon-button" type="button" data-header="menu" aria-haspopup="menu" aria-expanded="${open("menu")}" aria-label="Menu">${icon("menu")}</button>
               ${dropdown("menu")}
           </div>`;
    return `<header class="site-header"><div class="site-header-inner">
        <a class="logo" href="home.html">${icon("flame")}<span class="logo-word">TeamTavern</span></a>
        <div class="header-wrap">
            <button class="button button-text" type="button" data-header="games" aria-haspopup="dialog" aria-expanded="${open("games")}">Games${icon("chevron-down")}</button>
            ${dropdown("games")}
        </div>
        <div class="site-header-actions">
            <a class="button button-outline button-small" href="${options.newPostHref}" aria-label="New post">${icon("plus")}<span${signedIn() ? " class=\"hide-phone\"" : ""}>New post</span></a>
            ${right}
        </div>
    </div></header>
    ${headerOverlayHtml(options)}`;
};

const paintHeader = () => {
    const root = document.getElementById("header");
    if (root) root.innerHTML = headerHtml();
};

// Opening a menu paints the header with it open and puts the focus on its
// first item; closing gives the focus back to the button that opened it. A
// click anywhere else closes it, and so does Escape.

// Only a phone's full-screen menu holds the page behind it still, and it
// releases only what it took: another overlay may be holding the same lock.
let headerMenuLocked = false;

const paintHeaderMenu = () => {
    paintHeader();
    const lock = !!headerMenu && phoneWidth.matches;
    if (lock === headerMenuLocked) return;
    headerMenuLocked = lock;
    document.body.style.overflow = lock ? "hidden" : "";
};

const openHeaderMenu = name => {
    headerMenu = name;
    paintHeaderMenu();
    const root = document.querySelector(".header-menu");
    const first = root && (root.querySelector(".overlay-body") || root).querySelector("a[href], button");
    // A menu with nothing of its own to focus falls back to its Close, and a
    // dropdown to the button that opened it, which the repaint replaced.
    (first || root?.querySelector("[data-header=close]")
        || document.querySelector(`.site-header [data-header="${name}"]`))?.focus();
};

const closeHeaderMenu = (returnFocus = true) => {
    if (!headerMenu) return;
    const opener = headerMenu;
    headerMenu = null;
    paintHeaderMenu();
    if (returnFocus) document.querySelector(`.site-header [data-header="${opener}"]`)?.focus();
};

// Signing out lands on the home page: the page the player was on may have been
// theirs, and signed out the home page is what the site is for.
const signOut = () => {
    switchAccount("out");
    location.href = "home.html";
};

document.addEventListener("click", event => {
    // Only the header's own controls act; the components sheet shows copies of
    // the same menus, which are there to be read.
    const trigger = event.target.closest(".site-header [data-header], .header-menu [data-header], .backdrop[data-header]");
    const action = trigger ? trigger.dataset.header : null;
    const wasOpen = headerMenu;
    // A click outside the open menu closes it, its own button included, which
    // is what makes that button a toggle.
    if (headerMenu && !event.target.closest(".header-menu")) closeHeaderMenu(action === headerMenu);
    if (!action || action === wasOpen) return;
    if (action === "close") {
        closeHeaderMenu();
    } else if (action === "read-all") {
        // The list stays open, with the count on the bell behind it gone; the
        // button goes with the last unread row, so the focus moves to the
        // first of them.
        readNotifications(visibleNotifications().map(n => n.id));
        paintHeaderMenu();
        (document.querySelector(".header-menu [data-notification]")
            || document.querySelector(`.site-header [data-header="notifications"]`))?.focus();
    } else if (action === "sign-out") {
        closeHeaderMenu(false);
        signOut();
    } else {
        openHeaderMenu(action);
    }
});

// Opening a notification reads it. Only the header's own list acts; the
// components sheet shows a copy of it, which is there to be read.
document.addEventListener("click", event => {
    const row = event.target.closest(".site-header [data-notification], .header-menu [data-notification]");
    if (row) readNotifications([row.dataset.notification]);
});

document.addEventListener("keydown", event => {
    if (event.key === "Escape" && headerMenu) closeHeaderMenu();
});

// A menu drawn for one width has no place at the other, so crossing the
// breakpoint closes it.
phoneWidth.addEventListener("change", () => closeHeaderMenu(false));

// The prototype bar: not part of the design. It picks who is viewing, and pages
// add their own controls through pageBarExtras() and messagingBarExtras().

const prototypeBarHtml = () => {
    const as = !account ? "out" : account.id;
    const option = (value, label) => `<option value="${value}"${as === value ? " selected" : ""}>${label}</option>`;
    const extras = [
        typeof pageBarExtras === "function" ? pageBarExtras() : "",
        typeof messagingBarExtras === "function" ? messagingBarExtras() : "",
    ].join("");
    return `<strong>Prototype</strong>
        <label>Viewing as <select data-proto="as">
            ${option("out", "Signed out")}
            ${option("kestrel", "Kestrel, who has a Valorant group post")}
            ${option("vex", "Vex, before a first post")}
            ${as === "new" ? option("new", `${escapeHtml(account.nickname || account.discordName)}, just signed up`) : ""}
        </select></label>
        ${extras}
        <button type="button" data-proto="reset">Start over</button>`;
};

const paintPrototypeBar = () => {
    const root = document.getElementById("prototype-bar");
    if (root) root.innerHTML = prototypeBarHtml();
};

const paintChrome = () => {
    paintPrototypeBar();
    paintHeader();
};

// Start over forgets everything the prototype stored, the conversations
// included, and opens the page again with only its game.
const startOver = () => {
    Object.keys(localStorage).filter(k => k.startsWith("tt-draft-")).forEach(k => localStorage.removeItem(k));
    [ACCOUNT_KEY, PUBLISHED_KEY, FACTS_KEY, CONVERSATIONS_KEY, NOTIFICATIONS_KEY, BLOCKS_KEY, REPORTS_KEY, REVEALS_KEY, EMAILS_KEY, CLOCK_KEY]
        .forEach(k => localStorage.removeItem(k));
    sessionStorage.clear();
    const game = new URLSearchParams(location.search).get("game");
    location.href = `${location.pathname.split("/").pop()}${game ? `?game=${game}` : ""}`;
};

document.addEventListener("change", event => {
    if (event.target.dataset.proto !== "as") return;
    switchAccount(event.target.value);
    location.reload();
});

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) return;
    if (target.dataset.proto === "reset") {
        startOver();
    } else if (target.dataset.toast) {
        const id = target.dataset.toast;
        document.getElementById(id)?.remove();
        toastActions[id]?.();
        delete toastActions[id];
    }
});

// An open dialog keeps the focus: Tab past its last control comes back to its
// first, and Shift+Tab the other way.
document.addEventListener("keydown", event => {
    if (event.key !== "Tab") return;
    const dialogs = document.querySelectorAll("[role=dialog][aria-modal=true]");
    const dialog = dialogs[dialogs.length - 1];
    if (!dialog) return;
    const focusable = [...dialog.querySelectorAll("a[href], button:not(:disabled), input:not(:disabled), select, textarea, summary")]
        .filter(el => el.offsetParent !== null);
    if (!focusable.length) return;
    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    if (!dialog.contains(document.activeElement)) {
        event.preventDefault();
        first.focus();
    } else if (event.shiftKey && document.activeElement === first) {
        event.preventDefault();
        last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault();
        first.focus();
    }
});
