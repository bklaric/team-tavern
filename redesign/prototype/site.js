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

const readJson = (key, fallback) => {
    try {
        return JSON.parse(localStorage.getItem(key)) ?? fallback;
    } catch {
        return fallback;
    }
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
        unread: { notifications: 3 },
        posts: [
            { game: "valorant", type: "group", updated: "2026-09-06T19:12:00Z", draft: NIGHT_OWLS },
            { game: "dota2", type: "player", updated: "2026-07-20T17:40:00Z" },
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

const ACCOUNT_KEY = "tt-proto-account";
let account = readJson(ACCOUNT_KEY, null);
const saveAccount = () => localStorage.setItem(ACCOUNT_KEY, JSON.stringify(account));
const useAccount = id => {
    account = id === "out" ? null : structuredClone(PRESETS[id]);
    saveAccount();
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

// Signing up with Discord makes an account without a nickname; the register
// step asks for one.
const signedIn = () => !!(account && account.nickname);
const me = () => (signedIn() ? account.nickname : null);

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

// The header (brief 11.4). Pages may define pageHeaderOptions() returning
// { newPostHref, signInHref, signUpHref, current }.

// The site's one sign-up screen is the post flow's register step. Reached from
// anywhere else, it returns the player to where they were.
const signUpHref = (mode, extra = {}) => {
    const params = new URLSearchParams({ step: "register", ...(mode ? { mode } : {}), next: location.href.split("/").pop(), ...extra });
    return `post.html?${params}`;
};

const headerHtml = () => {
    const options = { newPostHref: "post.html", ...(typeof pageHeaderOptions === "function" ? pageHeaderOptions() : {}) };
    const count = n => n ? `<span class="badge" aria-hidden="true">${n}</span>` : "";
    const messages = unreadConversations();
    const notifications = ((account && account.unread) || {}).notifications;
    const right = signedIn()
        ? `<a class="icon-button header-count" href="messages.html" aria-label="Messages${messages ? `, ${messages} unread` : ""}"${options.current === "messages" ? ` aria-current="page"` : ""}>${icon("mail")}${count(messages)}</a>
           <a class="icon-button header-count" href="#" aria-label="Notifications${notifications ? `, ${notifications} new` : ""}">${icon("bell")}${count(notifications)}</a>
           <button class="avatar" type="button" aria-label="Account menu">${escapeHtml(account.nickname[0].toUpperCase())}</button>`
        : `<a class="button button-text button-small" href="${options.signInHref || signUpHref("signin")}">Sign in</a>
           <a class="button button-text button-small hide-phone" href="${options.signUpHref || signUpHref()}">Sign up</a>`;
    return `<header class="site-header"><div class="site-header-inner">
        <a class="logo" href="#">${icon("flame")}<span class="logo-word">TeamTavern</span></a>
        <button class="button button-text" type="button">Games${icon("chevron-down")}</button>
        <div class="site-header-actions">
            <a class="button button-outline button-small" href="${options.newPostHref}" aria-label="New post">${icon("plus")}<span${signedIn() ? " class=\"hide-phone\"" : ""}>New post</span></a>
            ${right}
        </div>
    </div></header>`;
};

const paintHeader = () => {
    const root = document.getElementById("header");
    if (root) root.innerHTML = headerHtml();
};

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
    [ACCOUNT_KEY, CONVERSATIONS_KEY, BLOCKS_KEY, REPORTS_KEY, CLOCK_KEY].forEach(k => localStorage.removeItem(k));
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
