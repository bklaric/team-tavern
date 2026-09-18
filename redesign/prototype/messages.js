// The inbox (brief 10): conversations grouped by the post they are about, the
// viewer's own posts first, which tells an owner what their posts produced.
// On a desktop the open conversation sits beside the list; below that it has
// the screen to itself. messages.html?c=<conversation id> opens one.

let selected = new URLSearchParams(location.search).get("c");

const lastAt = c => new Date(c.messages.at(-1).at);
const byLatest = (a, b) => lastAt(b) - lastAt(a);

// The viewer's own post as it is now, since they can renew it from here.
const ownPost = post => account.posts.find(p => postId(account, p) === post.id);
const asNow = post => ({ ...post, updated: (ownPost(post) || {}).updated || post.updated });

const factsLine = facts => `<div class="facts-clip"><div class="facts">${facts.map(fact).join("")}</div></div>`;

// The list.

const rowHtml = (c, title, lead) => inboxRowHtml(c, title, lead, selected);

// A post of the viewer's heads its conversations, one per player who wrote
// about it. An expired one can be renewed from here.
const ownGroupsHtml = conversations => {
    const groups = new Map();
    conversations.forEach(c => groups.set(c.post.id, (groups.get(c.post.id) || []).concat(c)));
    return [...groups.values()]
        .map(group => group.sort(byLatest))
        .sort((a, b) => byLatest(a[0], b[0]))
        .map(group => {
            const post = asNow(group[0].post);
            const expired = postExpired(post);
            const count = `${group.length} ${group.length === 1 ? "conversation" : "conversations"}`;
            return `<div class="inbox-group">
                <div class="inbox-post">
                    <img class="inbox-cover" src="${coverOf(post.game)}" alt="">
                    <span class="inbox-post-text">
                        <span class="inbox-post-name">${escapeHtml(postName(post))}</span>
                        <span class="inbox-kind">${escapeHtml(postKind(post))} · ${count}${expired ? " · Expired" : ""}</span>
                    </span>
                    ${expired ? `<button class="button button-outline button-small" type="button" data-renew="${escapeHtml(post.id)}">${icon("refresh-cw")}Renew</button>` : ""}
                </div>
                ${group.map(c => rowHtml(c, escapeHtml(c.starter.nickname))).join("")}
            </div>`;
        }).join("");
};

const messagedHtml = conversations => conversations.sort(byLatest).map(c => rowHtml(c,
    `${escapeHtml(postName(c.post))} <span class="inbox-kind">· ${escapeHtml(postKind(c.post))}</span>`,
    `<img class="inbox-cover" src="${coverOf(c.post.game)}" alt="">`)).join("");

const listHtml = () => {
    const all = visibleConversations();
    const mine = all.filter(c => c.post.owner === me());
    const theirs = all.filter(c => c.starter.nickname === me());
    return `<nav class="inbox" aria-labelledby="inbox-title">
        <h1 id="inbox-title">Messages</h1>
        ${mine.length ? `<h2 class="inbox-heading">Your posts</h2>${ownGroupsHtml(mine)}` : ""}
        ${theirs.length ? `<h2 class="inbox-heading">Posts you messaged</h2>${messagedHtml(theirs)}` : ""}
    </nav>`;
};

// The open conversation. With a player about the viewer's own post, the
// header is that player: their post's facts if they have one in the game. About
// someone else's post, the header is the post, with its owner's contacts a
// click away.

const theirPostLine = post => !post ? ""
    : post.type === "player" ? factsLine(post.facts)
    : factsLine([{ text: `Their ${post.type} ${postName(post)}` }].concat(post.facts));

const conversationHeaderHtml = c => {
    const own = c.post.owner === me();
    const other = otherIn(c, me());
    const post = asNow(c.post);
    const back = `<a class="icon-button messages-back" href="messages.html" data-conversation="" aria-label="All messages">${icon("arrow-left")}</a>`;
    const title = own ? other : postName(post);
    const lines = own
        ? `${theirPostLine(c.starter.post)}
           <div class="conversation-context">About your post ${escapeHtml(postName(post))} · ${escapeHtml(postKind(post))}${
               postExpired(post) ? ` · Expired <button class="button button-text button-small" type="button" data-renew="${escapeHtml(post.id)}">${icon("refresh-cw")}Renew</button>` : ""}</div>`
        : `<div class="conversation-context">${escapeHtml(postKind(post))} · ${
               post.type === "player" ? `Active ${ago(new Date(post.updated))}` : `${post.type === "community" ? "Run by" : "Posted by"} ${escapeHtml(post.owner)}`}</div>
           ${factsLine(post.facts)}
           ${post.contacts.length ? `<details class="thread-contacts">
               <summary>${post.type === "community" ? "Ways to join" : `${escapeHtml(post.owner)}'s contacts`}${icon("chevron-down")}</summary>
               ${contactRows(post.contacts)}
           </details>` : ""}`;
    return `<div class="conversation-header">
        ${back}
        <div class="conversation-title">
            <h2 id="conversation-title">${escapeHtml(title)}</h2>
            ${lines}
        </div>
        ${menuHtml(messaging.menu, other, messaging.reportLabel)}
    </div>`;
};

const conversationHtml = c => {
    const own = c.post.owner === me();
    const body = messaging.view === "block" ? blockHtml(messaging.who)
        : messaging.view === "report" ? reportHtml(messaging.reportSubject, messaging.who)
        : `${own ? "" : olderNote(c.post)}${threadHtml(c, messaging.unreadFrom)}`;
    return `<section class="conversation" aria-labelledby="conversation-title">
        ${conversationHeaderHtml(c)}
        <div class="conversation-body thread-scroll">${body}</div>
        ${messaging.view === "main" ? `<div class="conversation-composer">${composerHtml(true)}</div>` : ""}
    </section>`;
};

// The page.

const pageHtml = () => {
    if (!signedIn()) {
        return `<div class="messages-alone"><div class="empty-state">
            <h2>Sign in to see your messages</h2>
            <p>Conversations about your posts, and about the posts you message, are kept here.</p>
            <a class="button button-primary" href="${signUpHref("signin")}">Sign in</a>
        </div></div>`;
    }
    if (!visibleConversations().length) {
        return `<div class="messages-alone"><div class="empty-state">
            <h2>No messages yet</h2>
            <p>Message someone from a game's feed, or publish a post: players who fit it can message you, and the conversation shows up here.</p>
            <a class="button button-primary" href="post.html">Publish a post</a>
        </div></div>`;
    }
    const c = messaging && messaging.conversation();
    return `<div class="messages-page"${c ? " data-open" : ""}>
        ${listHtml()}
        ${c ? conversationHtml(c) : `<section class="conversation"><div class="conversation-empty">Choose a conversation.</div></section>`}
    </div>`;
};

const paint = focus => {
    const c = messaging && messaging.conversation();
    if (c && messaging.view === "main") markRead(c);
    if (messaging) paintInto(document.getElementById("app"), pageHtml(), focus);
    else {
        document.getElementById("app").innerHTML = pageHtml();
        paintChrome();
    }
    document.title = c ? `${c.post.owner === me() ? otherIn(c, me()) : postName(c.post)} · Messages` : "Messages";
};

// Opens the selected conversation, or none. A conversation that is gone, such
// as one with a player the viewer blocked, opens nothing.
const openSelected = () => {
    const c = selected && conversationById(selected);
    if (!c) {
        selected = null;
        messaging = null;
        return;
    }
    const other = otherIn(c, me());
    openMessaging({
        post: c.post,
        who: other,
        reportSubject: other,
        reportLabel: `Report ${other}`,
        conversation: () => conversationById(c.id),
        paint,
        close: () => select(null),
        changed: () => paint(),
    });
};

const select = (id, push = true) => {
    selected = id || null;
    if (push) history.pushState(null, "", selected ? `messages.html?c=${encodeURIComponent(selected)}` : "messages.html");
    openSelected();
    paint(selected && matchMedia("(min-width: 1024px)").matches ? "composer" : null);
    if (!selected) window.scrollTo(0, 0);
};

document.addEventListener("click", event => {
    const row = event.target.closest("[data-conversation]");
    if (row && !event.metaKey && !event.ctrlKey) {
        event.preventDefault();
        select(row.dataset.conversation);
        return;
    }
    const renew = event.target.closest("[data-renew]");
    if (renew) {
        const stored = account.posts.find(p => postId(account, p) === renew.dataset.renew);
        stored.updated = NOW.toISOString();
        saveAccount();
        paint();
        toast(`Renewed. Your post stays active for ${lifetime(stored.type) / DAY} days from today.`);
    }
});

addEventListener("popstate", () => select(new URLSearchParams(location.search).get("c"), false));

const pageHeaderOptions = () => ({ current: "messages" });

openSelected();
paint();
