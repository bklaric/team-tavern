// The home page (brief 11.2). Signed out, or signed in without a post, it starts
// post creation: What are you posting?, then the cover grid into each game's
// feed. A signed-in player's home page is their posts: the games they have
// posts in, each with its cover and those posts beside it, then the other games.

const TYPE_ORDER = ["player", "group", "community"];

// Signed out, and signed in without a post.

const startHtml = () => `<div class="home">
    <section class="home-start" aria-labelledby="start-title">
        <h1 id="start-title">What are you posting?</h1>
        <p class="home-lead">Find players, groups and communities for the games you play. Post once, and we'll tell you when someone new fits.</p>
        ${typeCardsHtml(type => `post.html?type=${type}`)}
    </section>
    <section class="home-games" aria-labelledby="games-title">
        <h2 id="games-title">Or browse a game</h2>
        ${coverGridHtml(GAMES)}
    </section>
</div>`;

// A player's own post, as renderOwnPost takes it: the card it was published
// with, what the owner is told about it, and its own page, which its name opens.
const ownPost = stored => {
    const id = postId(account, stored);
    return {
        id,
        href: postPageHref(stored.game, id),
        type: stored.type,
        name: storedPostName(account, stored),
        ...(stored.card || { facts: [] }),
        ...ownPostStats(stored),
        fitsHref: feedHref(stored.game),
        editHref: `post.html?game=${stored.game}&type=${stored.type}&from=edit`,
    };
};

// A game the player has posts in: its cover, and their posts beside it in the
// order of the type chooser. The cover is the game's heading and names it. A
// game without all three types offers a new post for it below the last,
// starting at the type step with the game known.
const gameHtml = (game, posts) => {
    const title = gameTitle(game);
    const more = posts.length < TYPE_ORDER.length
        ? `<a class="button button-text button-small" href="post.html?game=${game}">${icon("plus")}New ${escapeHtml(title)} post</a>`
        : "";
    return `<section class="home-game" aria-labelledby="game-${game}">
        <h2 class="home-game-heading" id="game-${game}"><a class="home-game-cover" href="${feedHref(game)}">
            <img src="${coverOf(game)}" alt=""><span class="home-game-name">${escapeHtml(title)}</span>
        </a></h2>
        <div class="home-game-posts">
            ${posts
                .slice()
                .sort((a, b) => TYPE_ORDER.indexOf(a.type) - TYPE_ORDER.indexOf(b.type))
                .map(stored => renderOwnPost(ownPost(stored))).join("")}
            ${more}
        </div>
    </section>`;
};

// Games keep the cover grid's order, so renewing a post doesn't move it.
const postsHtml = () => {
    const withPosts = GAMES.filter(g => account.posts.some(p => p.game === g.handle));
    const others = GAMES.filter(g => !withPosts.includes(g));
    return `<div class="home">
        <h1>Your posts</h1>
        ${withPosts.map(g => gameHtml(g.handle, account.posts.filter(p => p.game === g.handle))).join("")}
        ${others.length ? `<section class="home-games" aria-labelledby="games-title">
            <h2 id="games-title">Other games</h2>
            ${coverGridHtml(others)}
        </section>` : ""}
    </div>`;
};

const hasPosts = () => signedIn() && account.posts.length > 0;

const paint = () => {
    paintChrome();
    document.getElementById("app").innerHTML = hasPosts() ? postsHtml() : startHtml();
    document.title = hasPosts() ? "Your posts · TeamTavern" : "TeamTavern: find players, groups and communities";
};

// See what fits opens the game's feed with the description taken from the
// post. A player post describes its owner with the facts their account holds.
const PLAYER_FACTS = ["location", "languages", "birthday"];

const seeWhatFits = id => {
    const stored = account.posts.find(p => postId(account, p) === id);
    const draft = { ...(stored.draft || {}) };
    if (stored.type === "player") PLAYER_FACTS.forEach(key => { if (account[key] !== undefined) draft[key] = account[key]; });
    describeFeed(stored.game, stored.type, describedBy(stored.type, draft));
};

// Renewing makes a post active again from today, at any time (brief 9).
const renew = id => {
    const stored = account.posts.find(p => postId(account, p) === id);
    stored.updated = NOW.toISOString();
    saveAccount();
    paint();
    document.querySelector(`[data-renew="${CSS.escape(id)}"]`).focus();
    toast(`Renewed. Your post stays active for ${lifetime(stored.type) / DAY} days from today.`);
};

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) return;
    if (target.dataset.fits) seeWhatFits(target.dataset.fits);
    else if (target.dataset.renew) renew(target.dataset.renew);
    else if (target.getAttribute("href") === "#") event.preventDefault();
});

paint();
