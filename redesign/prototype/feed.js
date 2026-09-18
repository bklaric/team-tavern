// The feed prototype (brief sections 4, 5 and 7): one game's posts from
// data/<handle>.js, the viewer's description, matching, tiers, the expired
// divider and Load more.

// The description (brief 7.1): one post type and that type's fields, kept in
// local storage per game.

const STORAGE_KEY = `tt-description-${GAME.handle}`;
const emptyDescription = () => ({ type: "player", player: {}, group: {}, community: {} });

const loadDescription = () => {
    const preset = new URLSearchParams(location.search).get("describe");
    if (preset) return { ...emptyDescription(), ...JSON.parse(preset) };
    try {
        return JSON.parse(localStorage.getItem(STORAGE_KEY)) || emptyDescription();
    } catch {
        return emptyDescription();
    }
};

let description = loadDescription();
const saveDescription = () => localStorage.setItem(STORAGE_KEY, JSON.stringify(description));
const current = () => description[description.type];
const descriptionIsEmpty = () =>
    descriptionFields(description.type).every(f => isEmpty(current()[f.key]));

// The feed: active posts in tiers, then the divider, then expired posts in the
// same order without headings (brief 4 and 7.2).

let segment = "all";
let shown = BATCH;

const SEGMENTS = [
    { value: "all", label: "All" },
    { value: "player", label: "Players" },
    { value: "group", label: "Groups" },
    { value: "community", label: "Communities" },
];

const visibleTypes = () =>
    description.type !== "player" ? ["player"]
    : segment === "all" ? ["player", "group", "community"]
    : [segment];

// A tier counts the fields that apply to the post's type and don't match. A post
// with none of the viewer's fields to compare goes to the last tier.
const missesOf = m => (m.compared === 0 ? Infinity : m.misses);
const tierOf = m => Math.min(missesOf(m), 2);
const TIER_HEADINGS = ["Fits you", "Missing one thing", "Missing more"];

// The viewer's own posts are in the feed only while the description is empty,
// and a blocked player's posts never are (brief 4 and 10).
const ordered = () => {
    const empty = descriptionIsEmpty();
    const entries = POSTS
        .filter(p => visibleTypes().includes(p.type) && !hiddenFromViewer(p.owner) && (empty || !p.own))
        .map(post => ({ post, m: empty ? {} : compare(post, description.type, current()) }));
    const byOrder = (a, b) =>
        (empty ? 0 : missesOf(a.m) - missesOf(b.m) || 0) || b.post.updated - a.post.updated;
    const active = entries.filter(e => !e.post.expired).sort(byOrder);
    const expired = entries.filter(e => e.post.expired).sort(byOrder);
    return { empty, active, expired };
};

const renderFeed = () => {
    const { empty, active, expired } = ordered();
    const tierCounts = [0, 0, 0];
    active.forEach(e => tierCounts[tierOf(e.m)]++);
    const all = active.concat(expired);
    const html = [];
    let tier = null;
    let stack = [];
    const flush = () => {
        if (stack.length) html.push(`<div class="feed-stack">${stack.join("")}</div>`);
        stack = [];
    };
    if (!active.length) html.push(`<div class="divider">Older posts · they may no longer be looking</div>`);
    all.slice(0, shown).forEach((e, i) => {
        if (i === active.length && active.length) {
            flush();
            html.push(`<div class="divider">Older posts · they may no longer be looking</div>`);
        }
        if (!empty && !e.post.expired && tierOf(e.m) !== tier) {
            flush();
            tier = tierOf(e.m);
            html.push(`<h3 class="tier-heading">${TIER_HEADINGS[tier]} <span class="tier-count tabular">${tierCounts[tier]}</span></h3>`);
        }
        stack.push(renderCard(withViewer(toCard(e.post, e.m), e.post), !empty));
    });
    flush();
    document.getElementById("feed").innerHTML = html.join("");
    document.getElementById("load-more").innerHTML = all.length > shown
        ? `<button class="button button-outline" type="button" data-action="load-more">Load more</button>`
        : "";
};

// The description bar on a desktop: the type, then a chip per field that opens
// its editor in a popover. The feed follows every change.

const PHONE = matchMedia("(max-width: 639px)");
let openField = null;
let showMore = false;

const chip = f => {
    const value = current()[f.key];
    const text = summary(f, value);
    if (f.kind === "toggle") {
        return `<div class="field-chip-wrap"><button class="field-chip${value ? " field-chip-filled" : ""}" type="button" data-toggle="${f.key}" aria-pressed="${!!value}">${icon("mic")}${f.label}</button></div>`;
    }
    const open = openField === f.key;
    return `<div class="field-chip-wrap" data-field="${f.key}">
        <button class="field-chip${text ? " field-chip-filled" : ""}" type="button" data-chip="${f.key}" aria-expanded="${open}">
            ${escapeHtml(text || f.label)}${icon("chevron-down")}
        </button>
        ${open ? `<div class="popover" role="dialog" aria-label="${f.label}">${editor(f, value, "bar")}
            <div class="popover-footer">
                <button class="button button-text button-small" type="button" data-clear="${f.key}">Clear</button>
                <button class="button button-outline button-small" type="button" data-close>Done</button>
            </div></div>` : ""}
    </div>`;
};

const typeChoices = prefix => `<div class="choices" role="radiogroup" aria-label="You are">${TYPE_CHOICES.map(c =>
    `<label class="choice"><input type="radio" name="${prefix}-type" value="${c.type}"${description.type === c.type ? " checked" : ""}>${icon(c.icon)}${c.text}</label>`
).join("")}</div>`;

const renderBar = () => {
    const fields = descriptionFields(description.type);
    const primary = fields.filter(f => !f.more);
    const more = fields.filter(f => f.more);
    const moreFilled = more.some(f => has(current()[f.key]));
    const showing = showMore || moreFilled ? more : [];
    return `<section class="description" aria-labelledby="description-heading">
        <div class="description-heading">
            <h2 id="description-heading">Tell us about you</h2>
            <p>Posts that fit you come first.</p>
        </div>
        ${typeChoices("bar")}
        <div class="field-chips">
            ${primary.concat(showing).map(chip).join("")}
            ${more.length && !showing.length ? `<button class="field-chip" type="button" data-action="more">More${icon("chevron-down")}</button>` : ""}
            ${descriptionIsEmpty() ? "" : `<button class="button button-text button-small" type="button" data-action="clear-all">Clear all</button>`}
        </div>
    </section>`;
};

// On a phone the bar is a summary that opens a full-screen sheet, and the feed
// updates when the sheet closes (brief 7.1).

let sheetOpen = false;

const renderSummary = () => {
    const parts = descriptionFields(description.type).map(f => summary(f, current()[f.key])).filter(Boolean);
    const choice = TYPE_CHOICES.find(c => c.type === description.type);
    const text = parts.length ? parts.join(" · ") : "Tell us about you to see what fits";
    return `<button class="description-summary" type="button" data-action="open-sheet">
        ${icon(choice.icon)}<span>${escapeHtml(text)}</span>${icon("pencil")}
    </button>`;
};

const renderSheet = () => `<div class="overlay" role="dialog" aria-modal="true" aria-label="Tell us about you">
    <div class="overlay-header"><h2 style="font-size: 16px">Tell us about you</h2>
        <button class="icon-button" type="button" data-action="close-sheet" aria-label="Close">${icon("x")}</button></div>
    <div class="overlay-body">
        ${typeChoices("sheet")}
        ${descriptionFields(description.type).map(f => `<div class="sheet-field"><h3>${f.label}</h3>${editor(f, current()[f.key], "sheet")}</div>`).join("")}
    </div>
    <div class="overlay-footer"><button class="button button-primary" type="button" data-action="close-sheet">Show posts</button></div>
</div>`;

// The prompt follows a description that has something in it. A game with no
// active posts shows it regardless, since that is all there is to do there.
const renderPublishPrompt = () => {
    const quiet = !POSTS.some(p => !p.expired);
    if (descriptionIsEmpty() && !quiet) return "";
    const who = description.type === "player" ? "groups and players can find you too" : "players can find you too";
    const text = descriptionIsEmpty()
        ? `Nobody has posted for ${escapeHtml(GAME.title)} lately. Publish your post and we'll tell you when someone fits.`
        : `Publish this as your post: ${who}, and we'll tell you when someone new fits.`;
    return `<div class="publish-prompt">${icon("flame")}
        <p>${text}</p>
        <a class="button button-primary" data-action="publish" href="post.html?game=${GAME.handle}&type=${description.type}&from=feed">Publish post</a></div>`;
};

const renderSegments = () => description.type !== "player" ? "" :
    `<div class="segments" role="group" aria-label="Showing">${SEGMENTS.map(s =>
        `<button class="segment" type="button" data-segment="${s.value}" aria-pressed="${segment === s.value}">${s.label}</button>`
    ).join("")}</div>`;

const renderDescription = () => {
    document.getElementById("description").innerHTML = PHONE.matches ? renderSummary() : renderBar();
    document.getElementById("sheet").innerHTML = PHONE.matches && sheetOpen ? renderSheet() : "";
    document.body.style.overflow = PHONE.matches && sheetOpen ? "hidden" : "";
    document.getElementById("publish").innerHTML = renderPublishPrompt();
    document.getElementById("segments").innerHTML = renderSegments();
};

const render = () => {
    renderDescription();
    renderFeed();
};

// Changes.

const setValue = (key, value) => {
    current()[key] = value;
    saveDescription();
    shown = BATCH;
};

const setType = type => {
    description.type = type;
    segment = "all";
    openField = null;
    showMore = false;
    saveDescription();
    shown = BATCH;
};

document.addEventListener("input", event => {
    const editorElement = event.target.closest("[data-editor]");
    if (editorElement) {
        const f = descriptionFields(description.type).find(x => x.key === editorElement.dataset.editor);
        setValue(f.key, readEditor(editorElement, f));
        if (PHONE.matches) return;
        // Refresh the chip label in place, so the open editor keeps its focus.
        const button = document.querySelector(`[data-chip="${f.key}"]`);
        const text = summary(f, current()[f.key]);
        button.classList.toggle("field-chip-filled", !!text);
        button.innerHTML = `${escapeHtml(text || f.label)}${icon("chevron-down")}`;
        document.getElementById("publish").innerHTML = renderPublishPrompt();
        renderFeed();
        return;
    }
    if (event.target.name && event.target.name.endsWith("-type")) {
        setType(event.target.value);
        if (PHONE.matches) renderDescription();
        else render();
    }
});

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) {
        if (openField && !event.target.closest(".popover")) {
            openField = null;
            renderDescription();
        }
        return;
    }
    const data = target.dataset;
    if (data.chip) {
        openField = openField === data.chip ? null : data.chip;
        renderDescription();
        const first = document.querySelector(".popover input, .popover select");
        if (first) first.focus();
    } else if (data.toggle) {
        setValue(data.toggle, !current()[data.toggle]);
        render();
    } else if (data.clear) {
        setValue(data.clear, undefined);
        openField = null;
        render();
    } else if ("close" in data) {
        openField = null;
        renderDescription();
    } else if (data.segment) {
        segment = data.segment;
        shown = BATCH;
        render();
    } else if (data.action === "more") {
        showMore = true;
        renderDescription();
    } else if (data.action === "clear-all") {
        description[description.type] = {};
        saveDescription();
        shown = BATCH;
        render();
    } else if (data.action === "publish") {
        // The post screen reads the description from storage, however it was set.
        saveDescription();
    } else if (data.card === "edit") {
        const post = POSTS.find(p => p.id === target.closest(".card").dataset.id);
        location.href = `post.html?game=${GAME.handle}&type=${post.type}&from=edit`;
    } else if (data.card === "renew") {
        renew(target.closest(".card").dataset.id);
    } else if (data.action === "load-more") {
        shown += BATCH;
        renderFeed();
    } else if (data.action === "open-sheet") {
        sheetOpen = true;
        renderDescription();
    } else if (data.action === "close-sheet") {
        sheetOpen = false;
        render();
    } else if (!target.closest(".field-chip-wrap") && openField) {
        openField = null;
        renderDescription();
    }
});

document.addEventListener("keydown", event => {
    if (event.key !== "Escape") return;
    if (openField) {
        const field = openField;
        openField = null;
        renderDescription();
        document.querySelector(`[data-chip="${field}"]`).focus();
    } else if (sheetOpen) {
        sheetOpen = false;
        render();
    }
});

PHONE.addEventListener("change", () => {
    sheetOpen = false;
    openField = null;
    render();
});

// The viewer's own posts: renewing one makes it active for another 30 days, or
// 90 for a community (brief 9).
const renew = id => {
    const post = POSTS.find(p => p.id === id);
    const stored = account.posts.find(p => postId(account, p) === id);
    stored.updated = NOW.toISOString();
    saveAccount();
    Object.assign(post, { updated: NOW, expired: false, freshness: `Active ${ago(NOW)}` });
    renderFeed();
    toast(`Renewed. Your post stays active for ${lifetime(post.type) / DAY} days from today.`);
};

const onMessagingChange = () => {
    renderFeed();
    paintHeader();
};

const pageHeaderOptions = () => ({ newPostHref: `post.html?game=${encodeURIComponent(GAME.handle)}` });

// The page.

const activeCount = POSTS.filter(p => !p.expired).length;
document.title = `${GAME.title}: find players, groups and communities`;
document.getElementById("feed-header").innerHTML = `
    <img class="feed-cover" src="../../src/TeamTavern/Client/Static/Images/Games/${GAME.handle}.webp" alt="">
    <div>
        <h1>${escapeHtml(GAME.title)}</h1>
        <p>Find players, groups and communities</p>
        <div class="feed-active tabular">${activeCount} active ${activeCount === 1 ? "post" : "posts"}</div>
    </div>`;
paintChrome();
render();
openPanelFromQuery();
