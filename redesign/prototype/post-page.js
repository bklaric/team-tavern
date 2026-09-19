// A post's own page (brief 11.1): the post as an expanded card, and a way into
// the game's feed, which shows the viewer what fits their own description. It
// is what a card's name, the home page, a match email and a notification link
// to, and the only public page about a player.

const POST_ID = new URLSearchParams(location.search).get("id");
const POST = POSTS.find(p => p.id === POST_ID);

// The post the account holds, when the viewer is its owner: what the owner is
// told about it comes from there, as on the home page.
const storedPost = () => signedIn() && account.posts.find(p => postId(account, p) === POST_ID);

// A blocked player can still reach a post's page through a link, and sees the
// post without its contact button (brief 10).
const blocked = () => !!POST && hiddenFromViewer(POST.owner);

// Search engines. An expired post's page is kept out of them while it is
// expired, and renewal removes the tag again.
const setRobots = value => {
    const head = document.head;
    let meta = head.querySelector("meta[name=robots]");
    if (!value) return meta && meta.remove();
    if (!meta) {
        meta = document.createElement("meta");
        meta.name = "robots";
        head.append(meta);
    }
    meta.content = value;
};

// The stand-ins: what the real site does where the prototype can't, such as
// the sitemap and the status code a crawler gets.
const standInNote = text =>
    `<p class="stand-in-note"><span class="stand-in-label">Stand-in</span><span>${text}</span></p>`;

// The post.

// A page can be opened from anywhere, a search result or a match email among
// them, so the card's type names the game there: "Valorant player".
const typeLabel = () => `${GAME.title} ${POST.type}`;

const cardHtml = () => {
    const stored = storedPost();
    return renderCard({
        ...withViewer(toCard(POST, {}), POST),
        page: true,
        typeLabel: typeLabel(),
        bare: blocked(),
        status: stored ? ownPostStatus(ownPostStats(stored)) : "",
    });
};

// A visitor is told an expired post may be dead before they write to it; its
// owner is told the same in their own terms by the post's state.
const notesHtml = () => {
    if (blocked()) {
        return `<p class="post-note">${icon("ban")}You blocked ${escapeHtml(POST.owner)}, so there's no way to contact this post. <a href="account.html#blocked">Unblock from your account</a> to bring it back.</p>`;
    }
    if (POST.expired && !storedPost()) {
        return `<p class="post-note">${icon("info")}This is an older post. ${escapeHtml(POST.owner)} may no longer be looking, but you can still write.</p>`;
    }
    return "";
};

// The way into the feed. For a visitor it is the game's feed, which puts what
// fits their description first; for the owner it is See what fits, which takes
// the description from this post, as the home page does (brief 11.2).

const descriptionSummary = () => {
    const stored = readJson(`tt-description-${GAME.handle}`, null);
    if (!stored) return null;
    const described = stored[stored.type] || {};
    const parts = descriptionFields(stored.type).map(f => summary(f, described[f.key])).filter(Boolean);
    return parts.length ? { type: stored.type, text: parts.join(" · ") } : null;
};

const feedSectionHtml = () => {
    const own = !!storedPost();
    const active = POSTS.filter(p => !p.expired).length;
    const described = own ? null : descriptionSummary();
    const title = own ? `See what fits ${escapeHtml(postName(POST))}` : `More ${escapeHtml(GAME.title)} posts`;
    const line = own
        ? POST.type === "player"
            ? "Groups, communities and players that fit it come first."
            : "Players who fit it come first."
        : described
        ? "Posts that fit you come first."
        : "Tell us about you, and the posts that fit come first.";
    return `<section class="post-feed" aria-labelledby="post-feed-title">
        <img class="feed-cover" src="${coverOf(GAME.handle)}" alt="">
        <div class="post-feed-text">
            <h2 id="post-feed-title">${title}</h2>
            <p>${line}</p>
            ${described ? `<p class="post-feed-described">${icon(TYPES[described.type].icon)}<span>${escapeHtml(described.text)}</span></p>` : ""}
            <div class="feed-active tabular">${active} active ${active === 1 ? "post" : "posts"}</div>
        </div>
        <a class="button button-outline" href="${feedHref(GAME.handle)}"${own ? ` data-action="fits"` : ""}>
            ${icon("search")}${own ? "See what fits" : described ? "See what fits you" : `Browse ${escapeHtml(GAME.title)} posts`}
        </a>
    </section>`;
};

// Back. The feed loads its posts a batch at a time, so leaving it for a post's
// page and coming back must not start it over: the link is the browser's own
// Back, and the feed puts its batches back (brief 11.1, feed.js). A page opened
// from a link, a match email or a search result has no feed behind it, and only
// the section below the post leads into one.
const feedIsBehind = () => {
    const [page, query] = previousPage.split("?");
    return page === "feed.html" && new URLSearchParams(query).get("game") === GAME.handle;
};

const backHtml = () => feedIsBehind()
    ? `<a class="button button-text button-small post-back" href="${feedHref(GAME.handle)}" data-action="back">${icon("arrow-left")}Back to ${escapeHtml(GAME.title)} posts</a>`
    : "";

// A deleted post's page says the post is gone and leads to the feed; crawlers
// get a 404 (brief 11.1).
const goneHtml = () => `<div class="post-gone">
    <h1>This post is gone</h1>
    <p class="muted">Whoever posted it deleted it. The ${escapeHtml(GAME.title)} feed has everyone else who is looking.</p>
    <a class="button button-primary" href="${feedHref(GAME.handle)}">${icon("search")}Browse ${escapeHtml(GAME.title)} posts</a>
</div>
${standInNote(`The server answers this page with 404, so search engines drop it. A post that only expired keeps its page, with a <code>noindex</code> tag until it is renewed.`)}`;

const pageHtml = () => `${backHtml()}
    ${notesHtml()}
    ${cardHtml()}
    ${POST.expired ? standInNote(`While the post is expired the page carries <code>&lt;meta name="robots" content="noindex"&gt;</code>, which crawlers get in the prerendered HTML, and the sitemap leaves it out. Renewing removes both.`) : ""}
    ${feedSectionHtml()}`;

const paint = () => {
    paintChrome();
    document.getElementById("app").innerHTML = POST ? pageHtml() : goneHtml();
    document.title = POST
        ? `${postName(POST)} · ${typeLabel()} · TeamTavern`
        : `This post is gone · ${GAME.title} · TeamTavern`;
    setRobots(POST && POST.expired ? "noindex" : null);
};

const pageHeaderOptions = () => ({ newPostHref: `post.html?game=${encodeURIComponent(GAME.handle)}` });

// The panel's changes, such as a first message, show on the card.
const onMessagingChange = () => paint();

// The owner's actions, as in the feed: Edit opens the post screen on the post,
// and Renew makes it active again from today (brief 9).
const renew = () => {
    const stored = storedPost();
    stored.updated = NOW.toISOString();
    saveAccount();
    Object.assign(POST, { updated: NOW, expired: false, freshness: `Active ${ago(NOW)}` });
    paint();
    document.querySelector("[data-card=renew]").focus();
    toast(`Renewed. Your post stays active for ${lifetime(POST.type) / DAY} days from today.`);
};

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) return;
    const data = target.dataset;
    if (data.action === "back") {
        // The feed is one page back, with everything it had loaded.
        event.preventDefault();
        history.back();
    } else if (data.action === "fits") {
        const stored = storedPost();
        describeFeed(GAME.handle, stored.type, describedBy(stored.type, withPerson(stored.type, stored.draft || {}, account)));
    } else if (data.card === "edit") {
        location.href = `post.html?game=${GAME.handle}&type=${POST.type}&from=edit`;
    } else if (data.card === "renew") {
        renew();
    }
});

paint();
openPanelFromQuery();
