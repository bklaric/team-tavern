// Renders prototype components from plain post objects. Classic script, so the
// pages open straight from disk.

const TYPES = {
    player: { label: "Player", icon: "user" },
    group: { label: "Group", icon: "users" },
    community: { label: "Community", icon: "castle" },
};

const escapeHtml = text =>
    String(text).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);

const icon = (name, className = "") =>
    `<svg class="icon ${className}" viewBox="0 0 24 24" aria-hidden="true">${ICONS[name]}</svg>`;

const markIcon = match => icon(match === "fit" ? "check" : "equal-not", "fact-mark");

const markLabel = match =>
    `<span class="visually-hidden">${match === "fit" ? "Fits:" : "Doesn't fit:"}</span>`;

// A group's size sits in the heading beside its type: how many it is and how
// many more it wants, "3 players, wants 2 more", or a range where either will
// do (brief 5.2). A group that says only how many more it wants opens with
// that.
const slots = s => {
    if (!s) return "";
    const wanted = s.wantedFrom && s.wantedTo && s.wantedTo !== s.wantedFrom
        ? `${s.wantedFrom}–${s.wantedTo}`
        : s.wantedFrom || s.wantedTo;
    const size = s.size ? `${s.size} player${s.size === 1 ? "" : "s"}` : "";
    const wants = wanted ? `${size ? "wants" : "Wants"} ${wanted} more` : "";
    const text = [size, wants].filter(Boolean).join(", ");
    return text ? `<span class="card-slots tabular">${escapeHtml(text)}</span>` : "";
};

// A fact is { text } or { icon, label }, with an optional match of "fit" or
// "miss" that marks the fact itself.
const fact = f => {
    const match = f.match ? ` fact-${f.match}` : "";
    const mark = f.match ? markIcon(f.match) + markLabel(f.match) : "";
    const body = f.icon
        ? `${icon(f.icon)}<span class="visually-hidden">${escapeHtml(f.label)}</span>`
        : `<span class="${f.tabular ? "tabular" : ""}">${escapeHtml(f.text)}</span>`;
    return `<span class="fact${match}">${mark}${body}</span>`;
};

const heading = post => {
    const type = TYPES[post.type];
    const name = post.name || `${post.owner}'s ${post.type}`;
    // On its own page the card is the page, so its name is the page's heading
    // and links nowhere (brief 11.1).
    return `<div class="card-heading">
        ${post.page
            ? `<h1 class="card-name">${escapeHtml(name)}</h1>`
            : `<a class="card-name" href="${post.href || "#"}">${escapeHtml(name)}</a>`}
        <span class="card-type">${icon(type.icon)}${post.typeLabel || type.label}</span>
        ${slots(post.slots)}
        ${post.own ? `<span class="card-own">Your post</span>` : ""}
        <span class="card-freshness">${escapeHtml(post.freshness)}</span>
    </div>`;
};

const details = post => {
    const rows = (post.details || []).map(d =>
        `<div class="detail"><span class="detail-label">${escapeHtml(d.label)}</span><span>${escapeHtml(d.value)}</span></div>`);
    // A tracker is { title, url }, or a bare title that links nowhere.
    const trackers = (post.trackers || []).map(t => (typeof t === "string" ? { title: t, url: "#" } : t)).map(t =>
        `<div class="detail"><span class="detail-label">Tracker</span><a href="${escapeHtml(t.url)}"${t.url === "#" ? "" : ` target="_blank" rel="noopener"`}>${escapeHtml(t.title)} ${icon("external-link")}</a></div>`);
    const all = rows.concat(trackers);
    return all.length ? `<div class="card-details">${all.join("")}</div>` : "";
};

const ownerLine = post => {
    if (post.type === "player") return "";
    const verb = post.type === "community" ? "Run by" : "Posted by";
    const who = post.preview ? "you" : escapeHtml(post.owner);
    return `<span class="card-owner">${verb} ${who}</span>`;
};

// A bare card has no actions: it stands beside the choice it is about.
const actions = post => {
    if (post.bare) return "";
    if (post.own) {
        return `<button class="button button-outline button-small" type="button" data-card="edit">${icon("pencil")}Edit</button>
            <button class="button button-outline button-small" type="button" data-card="renew">${icon("refresh-cw")}Renew</button>`;
    }
    // A card's contact button is outlined, so a feed of twenty cards doesn't
    // show twenty filled ones. A post's own page holds one card, and contacting
    // it is what the page is for, so there it is the page's one filled button.
    const weight = post.page ? "button-primary" : "button-outline button-small";
    if (post.messaged) {
        return `<button class="button ${weight} card-contact" type="button">${icon("message-circle")}Open conversation</button>`;
    }
    const c = post.contact;
    return `<button class="button ${weight} card-contact" type="button">${icon(c.icon)}${escapeHtml(c.label)}</button>`;
};

// A card expands only when there is more to show than it already does.
const expandable = post => (post.details && post.details.length) || (post.trackers && post.trackers.length) || post.long;

const footer = post => `<div class="card-footer">
        <div class="card-meta">
            ${ownerLine(post)}
            ${post.messaged ? `<span class="card-messaged">${icon("message-circle")}You messaged ${escapeHtml(post.messaged)}</span>` : ""}
        </div>
        ${actions(post)}
        ${!post.page && (expandable(post) || post.text) ? `<button class="button button-text button-small card-details-toggle" type="button" aria-expanded="${!!post.expanded}">
            Details ${icon("chevron-down")}
        </button>` : ""}
    </div>`;

// marked shows the facts' matches; without it the card reads as it does with an
// empty description, or on its owner's screens.
const renderCard = (post, marked = false) => {
    const classes = ["card", `card-${post.type}`];
    if (post.expired) classes.push("card-expired");
    // On its own page a post is an expanded card, with nothing left to expand.
    if (post.expanded || post.page) classes.push("card-expanded");
    const unmark = f => marked ? f : { ...f, match: undefined };
    // A compared-only fact, such as online hours, otherwise waits behind Details.
    const shownFacts = post.facts.filter(f => !f.comparedOnly || (marked && f.match));
    const facts = shownFacts.map(unmark).map(fact).join("");
    return `<article class="${classes.join(" ")}" data-id="${post.id || ""}">
        ${heading(post)}
        ${facts ? `<div class="facts-clip"><div class="facts">${facts}</div></div>` : ""}
        ${post.text ? `<p class="card-text">${escapeHtml(post.text)}</p>` : ""}
        ${details(post)}
        ${post.status || ""}
        ${footer(post)}
    </article>`;
};

// A player's own post on the home page (brief 11.2): its card's heading and
// facts, its state, what it has produced, and See what fits, Edit and Renew.
// post is { id, href, type, name, slots, facts, days, expired, conversations,
// unread, conversationsHref, reveals, expiredAgo, fitsHref, editHref }, where
// href is the post's own page and days is how many are left, a post in its last
// week being about to expire.
const EXPIRING_DAYS = 7;

// What a post's state says to its owner, on the home page, on the post's own
// page and in the notification list (brief 11.3): how long it stays active, or
// what expiry means now that it has.
const ownPostStateText = post => {
    if (post.expired) {
        return { icon: "clock", text: `Expired ${post.expiredAgo}`, note: "It's listed under older posts, and match emails are paused." };
    }
    const days = post.days;
    return days <= EXPIRING_DAYS
        ? { icon: "circle-alert", soon: true, text: days ? `Expires in ${days} ${days === 1 ? "day" : "days"}` : "Expires today" }
        : { icon: "clock", text: `Active for ${days} more days` };
};

const ownPostState = post => {
    const state = ownPostStateText(post);
    return `<span class="own-post-state${state.soon ? " own-post-state-soon" : ""}">${icon(state.icon)}${
        escapeHtml(state.text)}${state.note ? `. ${escapeHtml(state.note)}` : ""}</span>`;
};

const ownPostConversations = post => {
    const n = post.conversations;
    if (!n) return `<span class="own-post-stat">${icon("message-circle")}No conversations yet</span>`;
    return `<a class="own-post-stat" href="${post.conversationsHref}">${icon("message-circle")}${n} ${n === 1 ? "conversation" : "conversations"}${
        post.unread ? `<span class="own-post-unread">${post.unread} unread</span>` : ""}</a>`;
};

// The state, the conversations and the contact reveals, which only the owner
// sees: on the home page under the post's facts, and on the post's own page
// under the post itself.
const ownPostStatus = post => `<div class="own-post-status">
    ${ownPostState(post)}
    ${ownPostConversations(post)}
    ${post.reveals ? `<span class="own-post-stat">${icon("eye")}Contacts shown ${post.reveals} ${post.reveals === 1 ? "time" : "times"}</span>` : ""}
</div>`;

const renderOwnPost = post => {
    const type = TYPES[post.type];
    const soon = !post.expired && post.days <= EXPIRING_DAYS;
    const facts = post.facts.map(fact).join("");
    return `<article class="card own-post${post.expired ? " card-expired" : ""}" data-id="${escapeHtml(post.id)}">
        <div class="card-heading">
            <a class="card-name" href="${post.href || "#"}">${escapeHtml(post.name)}</a>
            <span class="card-type">${icon(type.icon)}${type.label}</span>
            ${slots(post.slots)}
        </div>
        ${facts ? `<div class="facts-clip"><div class="facts">${facts}</div></div>` : ""}
        ${ownPostStatus(post)}
        <div class="card-footer">
            <a class="button button-outline button-small" href="${post.fitsHref}" data-fits="${escapeHtml(post.id)}">${icon("search")}See what fits</a>
            <a class="button button-text button-small" href="${post.editHref}">${icon("pencil")}Edit</a>
            <button class="button ${post.expired || soon ? "button-outline" : "button-text"} button-small" type="button" data-renew="${escapeHtml(post.id)}">${icon("refresh-cw")}Renew</button>
        </div>
    </article>`;
};

const reducedMotion = () => matchMedia("(prefers-reduced-motion: reduce)").matches;

// The card grows in place: its height animates from the collapsed to the
// expanded size, or back.
document.addEventListener("click", event => {
    const toggle = event.target.closest(".card-details-toggle");
    if (!toggle) return;
    const card = toggle.closest(".card");
    const from = card.offsetHeight;
    const expanded = card.classList.toggle("card-expanded");
    toggle.setAttribute("aria-expanded", expanded);
    if (reducedMotion()) return;
    const to = card.offsetHeight;
    card.animate(
        [{ height: `${from}px`, overflow: "hidden" }, { height: `${to}px`, overflow: "hidden" }],
        { duration: 180, easing: "ease-out" });
});
