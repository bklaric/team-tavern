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

const pips = (members, total) =>
    `<span class="pips" aria-hidden="true">${
        Array.from({ length: total }, (_, i) => `<span class="pip${i < members ? "" : " pip-open"}"></span>`).join("")
    }</span>`;

// A group's size sits in the heading beside its type: "●●●○○ 3 of 5", or on a
// server game "Wants 2–3 more".
const slots = s => !s ? ""
    : s.wants
    ? `<span class="card-slots">Wants <span class="tabular">${escapeHtml(s.wants)}</span> more</span>`
    : `<span class="card-slots">${pips(s.members, s.total)}<span class="tabular">${s.members} of ${s.total}</span></span>`;

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
    return `<div class="card-heading">
        <a class="card-name" href="${post.href || "#"}">${escapeHtml(name)}</a>
        <span class="card-type">${icon(type.icon)}${type.label}</span>
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
        return `<button class="button button-outline button-small" type="button">${icon("pencil")}Edit</button>
            <button class="button button-outline button-small" type="button">${icon("refresh-cw")}Renew</button>`;
    }
    if (post.messaged) {
        return `<button class="button button-outline button-small" type="button">${icon("message-circle")}Open conversation</button>`;
    }
    const c = post.contact;
    return `<button class="button button-outline button-small card-contact" type="button">${icon(c.icon)}${escapeHtml(c.label)}</button>`;
};

// A card expands only when there is more to show than it already does.
const expandable = post => (post.details && post.details.length) || (post.trackers && post.trackers.length) || post.long;

const footer = post => `<div class="card-footer">
        <div class="card-meta">
            ${ownerLine(post)}
            ${post.messaged ? `<span class="card-messaged">${icon("message-circle")}You messaged ${escapeHtml(post.messaged)}</span>` : ""}
        </div>
        ${actions(post)}
        ${expandable(post) || post.text ? `<button class="button button-text button-small card-details-toggle" type="button" aria-expanded="${!!post.expanded}">
            Details ${icon("chevron-down")}
        </button>` : ""}
    </div>`;

// marked shows the facts' matches; without it the card reads as it does with an
// empty description, or on its owner's screens.
const renderCard = (post, marked = false) => {
    const classes = ["card", `card-${post.type}`];
    if (post.expired) classes.push("card-expired");
    if (post.expanded) classes.push("card-expanded");
    const unmark = f => marked ? f : { ...f, match: undefined };
    // A compared-only fact, such as online hours, otherwise waits behind Details.
    const shownFacts = post.facts.filter(f => !f.comparedOnly || (marked && f.match));
    const facts = shownFacts.map(unmark).map(fact).join("");
    return `<article class="${classes.join(" ")}" data-id="${post.id || ""}">
        ${heading(post)}
        ${facts ? `<div class="facts-clip"><div class="facts">${facts}</div></div>` : ""}
        ${post.text ? `<p class="card-text">${escapeHtml(post.text)}</p>` : ""}
        ${details(post)}
        ${footer(post)}
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
