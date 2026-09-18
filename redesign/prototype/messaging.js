// Contact and messaging (brief 5.6 and 10): the contact panel a card's button
// opens, the conversation thread it shares with the inbox, and blocking and
// reporting the other side. A post here is what a conversation keeps of it:
// { id, game, type, name, owner, updated, slots, facts, reach, contacts }, where
// reach is the owner's preference: message, offsite or either for a player or
// group, discord, website or message for a community.

const postName = post => post.name || `${post.owner}'s ${post.type}`;
const postKind = post => `${gameTitle(post.game)} ${post.type}`;
const postExpired = post => new Date(post.updated).getTime() + lifetime(post.type) <= NOW.getTime();
const postAbout = post => (post.type === "player" ? `${post.owner}'s post` : postName(post));

// A card as the viewer sees it: their own post offers Edit and Renew, and a post
// they have a conversation about says so (brief 5.6).
const withViewer = (card, post) => {
    if (post.own) return { ...card, own: true };
    const c = conversationAbout(post.id);
    return c ? { ...card, messaged: ago(new Date(c.messages[0].at)) } : card;
};

// Contacts.

const copyButton = value => `<button class="button button-outline button-small" type="button" data-msg="copy" data-value="${escapeHtml(value)}">${icon("copy")}Copy</button>`;

const contactRow = c => `<div class="contact-row">
    <span class="contact-label">${escapeHtml(c.label)}</span>
    ${c.url
        ? `<a class="contact-value" href="${escapeHtml(c.url)}" target="_blank" rel="noopener">${escapeHtml(c.value)}${icon("external-link")}</a>`
        : `<span class="contact-value">${escapeHtml(c.value)}</span>`}
    ${copyButton(c.value)}
</div>`;

const contactRows = contacts => `<div class="contact-rows">${contacts.map(contactRow).join("")}</div>`;

// The preferences that put contacts before the message box.
const OFFSITE = ["offsite", "discord", "website"];

const contactsHeading = post => ({
    offsite: post.contacts.some(c => c.label === "Discord") ? "Prefers Discord" : "Prefers adding in game",
    discord: "Join on Discord",
    website: "Join on their website",
})[post.reach];

// A community that says how to join gets that way as the panel's one filled
// button.
const joinButton = post => {
    const label = { discord: "Discord invite", website: "Website" }[post.reach];
    const link = label && post.contacts.find(c => c.label === label);
    if (!link) return "";
    return `<a class="button button-primary" href="${escapeHtml(link.url)}" target="_blank" rel="noopener">${
        post.reach === "discord" ? `${icon("discord")}Open the invite` : `${icon("external-link")}Visit site`}</a>`;
};

// The conversation.

const threadHtml = (c, unreadFrom = c.messages.length, viewer = me()) => {
    const items = [];
    let day = null;
    c.messages.forEach((m, i) => {
        const date = new Date(m.at);
        if (dayLabel(date) !== day) {
            day = dayLabel(date);
            items.push(`<div class="thread-day">${day}</div>`);
        }
        if (i === unreadFrom) items.push(`<div class="thread-new">New</div>`);
        const own = m.from === viewer;
        const who = own ? "You" : escapeHtml(m.from);
        items.push(`<div class="message${own ? " message-own" : ""}"><span class="visually-hidden">${who}: </span>${escapeHtml(m.text)}</div>`);
        // A run of messages from one side, close together, shares one line of
        // who and when.
        const next = c.messages[i + 1];
        const runEnds = !next || next.from !== m.from || i + 1 === unreadFrom
            || new Date(next.at) - date > 15 * 60000 || dayLabel(new Date(next.at)) !== day;
        if (runEnds) items.push(`<div class="message-meta${own ? " message-meta-own" : ""}" aria-hidden="true">${who} · ${timeOfDay(date)}</div>`);
    });
    return `<div class="thread">${items.join("")}</div>`;
};

const composerHtml = primary => `<form class="composer" data-msg-form="send">
    <textarea class="textarea" id="composer" rows="1" aria-label="Message" placeholder="Write a message…"></textarea>
    <button class="button ${primary ? "button-primary" : "button-outline"}" type="submit">Send</button>
</form>`;

const olderNote = post => postExpired(post)
    ? `<span class="field-note">${icon("info")}This is an older post. ${escapeHtml(post.owner)} may no longer be looking.</span>`
    : "";

// A row of the inbox. A row titled with the other player needs only "You:"
// before its last message; one titled with a post, which has lead, names
// whoever wrote last.
const inboxRowHtml = (c, title, lead = "", current = null, viewer = me()) => {
    const last = c.messages.at(-1);
    const unread = unreadIn(c, viewer) > 0;
    const sender = last.from === viewer ? "You: " : lead ? `${escapeHtml(last.from)}: ` : "";
    return `<a class="inbox-row${unread ? " inbox-row-unread" : ""}" href="messages.html?c=${encodeURIComponent(c.id)}" data-conversation="${escapeHtml(c.id)}"${c.id === current ? ` aria-current="page"` : ""}>
        ${lead}
        <span class="inbox-row-main">
            <span class="inbox-row-top">
                <span class="inbox-row-title">${title}</span>
                <span class="inbox-row-time">${ago(new Date(last.at))}</span>
            </span>
            <span class="inbox-row-snippet">${sender}${escapeHtml(last.text)}</span>
        </span>
        ${unread ? `<span class="unread-dot"></span><span class="visually-hidden">Unread</span>` : ""}
    </a>`;
};

// Block and report, from the menu of the panel or the thread.

const menuHtml = (open, who, reportLabel) => `<div class="menu-wrap">
    <button class="icon-button" type="button" data-msg="menu" aria-haspopup="menu" aria-expanded="${open}" aria-label="More">${icon("ellipsis")}</button>
    ${open ? `<div class="menu" role="menu">
        <button class="menu-item" type="button" role="menuitem" data-msg="report">${icon("flag")}${escapeHtml(reportLabel)}</button>
        <button class="menu-item menu-item-destructive" type="button" role="menuitem" data-msg="block">${icon("ban")}Block ${escapeHtml(who)}</button>
    </div>` : ""}
</div>`;

const blockHtml = who => `<div class="confirm" role="alertdialog" aria-labelledby="block-title" aria-describedby="block-text">
    <h3 id="block-title">Block ${escapeHtml(who)}?</h3>
    <p id="block-text">You won't see each other's posts, your conversations leave both inboxes, and neither of you hears about the other's new posts. Nothing is deleted: unblocking brings it all back.</p>
    <div class="confirm-actions">
        <button class="button button-destructive" type="button" data-msg="confirm-block">${icon("ban")}Block ${escapeHtml(who)}</button>
        <button class="button button-text" type="button" data-msg="cancel">Cancel</button>
    </div>
</div>`;

const REPORT_REASONS = [
    "Spam or advertising",
    "Harassment, hate or threats",
    "Selling accounts, boosting or cheats",
    "Something else",
];

const reportHtml = (subject, who) => `<form class="form form-report" data-msg-form="report" novalidate>
    <div class="panel-section">
        <h3>Report ${escapeHtml(subject)}</h3>
        <p class="muted">Reports go to the people who run TeamTavern.</p>
    </div>
    <div class="field" data-field="report-reason">
        <span class="field-label" id="report-reason-label">What's wrong?</span>
        <div class="choice-list" role="radiogroup" aria-labelledby="report-reason-label">${REPORT_REASONS.map(r =>
            `<label class="choice"><input type="radio" name="report-reason" value="${escapeHtml(r)}">${escapeHtml(r)}</label>`).join("")}</div>
    </div>
    <div class="field">
        <label class="field-label" for="report-text">Anything we should know?</label>
        <textarea class="textarea" id="report-text" rows="3"></textarea>
    </div>
    <label class="check"><input type="checkbox" name="report-block">Also block ${escapeHtml(who)}</label>
    <div class="confirm-actions">
        <button class="button button-primary" type="submit">Send report</button>
        <button class="button button-text" type="button" data-msg="cancel">Cancel</button>
    </div>
</form>`;

// What is open: the contact panel or the inbox's thread. Both show one post's
// conversation with the other side, who can be blocked or reported. Hosts give
// { post, who, reportSubject, reportLabel, conversation(), paint(focus), close(),
// changed() }.

let messaging = null;

const openMessaging = host => {
    messaging = { view: "main", menu: false, draft: "", ...host };
    const c = messaging.conversation();
    messaging.unreadFrom = c ? c.read[me()] || 0 : 0;
};

// The contact panel (brief 5.6): the contacts the owner shared and the
// conversation about the post. The owner's preference decides which comes
// first; an existing conversation always does. On a desktop it is a side
// panel, so the card it was opened from stays in view.

const messageSection = (post, c, first, unreadFrom, viewer) => `<section class="panel-section" aria-label="Conversation">
    ${first ? `<h3>${escapeHtml(c ? `Your conversation with ${post.owner}` : `Message ${post.owner}`)}</h3>` : ""}
    ${olderNote(post)}
    ${c ? `<div class="thread-well">${threadHtml(c, unreadFrom, viewer)}</div>`
        : `<p class="muted">Your message starts a conversation about ${escapeHtml(postAbout(post))}. Replies show up here and in your inbox.</p>`}
    ${composerHtml(first)}
</section>`;

const contactsSection = (post, first) => `<section class="panel-section" aria-label="Contacts">
    ${first ? `<h3>${contactsHeading(post)}</h3>${joinButton(post)}` : ""}
    ${contactRows(post.contacts)}
</section>`;

const panelBody = (post, c, unreadFrom, viewer = me()) => {
    if (!post.contacts.length) return messageSection(post, c, true, unreadFrom, viewer);
    const contactsLead = !c && OFFSITE.includes(post.reach);
    const offsite = post.type === "community" ? "or join directly" : `or add ${escapeHtml(post.owner)} off-site`;
    return contactsLead
        ? `${contactsSection(post, true)}<div class="rule">or message on TeamTavern</div>${messageSection(post, c, false, unreadFrom, viewer)}`
        : `${messageSection(post, c, true, unreadFrom, viewer)}<div class="rule">${offsite}</div>${contactsSection(post, false)}`;
};

const panelSubtitle = post => post.type === "player"
    ? `${postKind(post)} · Active ${ago(new Date(post.updated))}`
    : `${postKind(post)} · ${post.type === "community" ? "Run by" : "Posted by"} ${post.owner}`;

// The panel without its backdrop and dialog role, which the components sheet
// shows as it is.
const panelInnerHtml = (post, body, menuOpen = false) => `<div class="overlay-header panel-header">
        <div class="panel-title">
            <h2 id="panel-title">${escapeHtml(postName(post))}</h2>
            <p>${escapeHtml(panelSubtitle(post))}</p>
        </div>
        <div class="panel-tools">
            ${menuHtml(menuOpen, post.owner, "Report this post")}
            <button class="icon-button" type="button" data-msg="close" aria-label="Close">${icon("x")}</button>
        </div>
    </div>
    <div class="overlay-body">${body}</div>`;

const panelHtml = () => {
    const post = messaging.post;
    const body = messaging.view === "block" ? blockHtml(messaging.who)
        : messaging.view === "report" ? reportHtml(messaging.reportSubject, messaging.who)
        : panelBody(post, messaging.conversation(), messaging.unreadFrom);
    return `<div class="backdrop" data-msg="close"></div>
    <div class="overlay overlay-side contact-panel" role="dialog" aria-modal="true" aria-labelledby="panel-title">
        ${panelInnerHtml(post, body, messaging.menu)}
    </div>`;
};

const autosize = textarea => {
    textarea.style.height = "auto";
    textarea.style.height = `${Math.min(textarea.scrollHeight + 2, 160)}px`;
};

// Paints what is open into root, keeps the message being written, marks the
// conversation read and puts the focus where asked: "composer", "first" for
// the first control of the body, or "close".
const paintInto = (root, html, focus) => {
    root.innerHTML = html;
    const composer = root.querySelector("#composer");
    if (composer) {
        composer.value = messaging.draft;
        if (messaging.draft) autosize(composer);
    }
    const c = messaging.conversation();
    if (c && messaging.view === "main" && c.read[me()] !== c.messages.length) markRead(c);
    root.querySelectorAll(".thread-scroll, .thread-well").forEach(el => { el.scrollTop = el.scrollHeight; });
    paintHeader();
    paintPrototypeBar();
    const target = focus === "composer" ? composer
        : focus === "first" ? root.querySelector(".overlay-body input, .overlay-body button, .conversation-body input, .conversation-body button")
        : focus === "close" ? root.querySelector("[data-msg=close].icon-button")
        : focus === "cancel" ? root.querySelector("[data-msg=cancel]")
        : null;
    if (target) target.focus();
};

const messagingChanged = () => typeof onMessagingChange === "function" && onMessagingChange();

const closePanel = () => {
    const opener = messaging && messaging.post.id;
    messaging = null;
    document.getElementById("panel").innerHTML = "";
    document.body.style.overflow = "";
    paintPrototypeBar();
    const button = opener && document.querySelector(`.card[data-id="${CSS.escape(opener)}"] .card-contact`);
    if (button) button.focus();
};

const openPanel = post => {
    openMessaging({
        post,
        who: post.owner,
        reportSubject: postName(post),
        reportLabel: "Report this post",
        conversation: () => conversationAbout(post.id),
        paint: focus => paintInto(document.getElementById("panel"), panelHtml(), focus),
        close: closePanel,
        changed: messagingChanged,
    });
    if (post.contacts.length) reveal(post.id);
    document.body.style.overflow = "hidden";
    const messageFirst = messaging.conversation() || !post.contacts.length || !OFFSITE.includes(post.reach);
    messaging.paint(messageFirst ? "composer" : "close");
};

// Opening a contact panel signed out leads to the site's one sign-up screen,
// which comes back to the feed with the panel open (brief 6, step 4).
const contactClicked = id => {
    const post = typeof postInfoById === "function" && postInfoById(id);
    if (!post) return;
    if (!signedIn()) {
        const here = new URL(location.href);
        here.searchParams.set("contact", id);
        location.href = signUpHref(null, { next: `${here.pathname.split("/").pop()}${here.search}`, to: post.owner });
        return;
    }
    openPanel(post);
};

// ?contact=<post id> opens that post's panel, as the sign-up screen returns.
const openPanelFromQuery = () => {
    const params = new URLSearchParams(location.search);
    const id = params.get("contact");
    if (!id) return;
    params.delete("contact");
    history.replaceState(null, "", `${location.pathname.split("/").pop()}?${params}`);
    if (signedIn()) contactClicked(id);
};

// The prototype's control for messaging: a reply from the other side of the
// open conversation.

const CANNED_REPLIES = [
    "Sounds good! When are you usually on?",
    "Sure, sending you a request now.",
    "Thanks for the message. Let me check with the others and get back to you.",
];

const messagingBarExtras = () => {
    const c = messaging && messaging.conversation();
    return c ? `<button type="button" data-proto="reply">Reply as ${escapeHtml(otherIn(c, me()))}</button>` : "";
};

// Changes.

const send = () => {
    const text = messaging.draft.trim();
    if (!text) return;
    let c = messaging.conversation();
    if (!c) {
        c = {
            id: `c-${Date.now()}`,
            post: messaging.post,
            starter: { nickname: me(), post: typeof viewerPostIn === "function" ? viewerPostIn() : null },
            messages: [],
            read: {},
        };
        allConversations().push(c);
    }
    const to = otherIn(c, me());
    const emailed = sendMessage(c, me(), text);
    messaging.draft = "";
    messaging.unreadFrom = c.messages.length;
    messaging.paint("composer");
    messaging.changed();
    const renew = c.post.owner === to && postExpired(c.post) ? ", with a Renew button, since the post has expired" : "";
    toast(emailed ? `${to} gets an email about your message${renew}.` : `No email: ${to} hasn't read your last message yet.`, { standIn: true });
};

const confirmBlock = () => {
    const host = messaging;
    const who = host.who;
    block(who);
    host.close();
    host.changed();
    toast(`${who} is blocked.`, {
        action: "Undo",
        onAction: () => {
            unblock(who);
            host.changed();
            toast(`${who} is unblocked.`);
        },
    });
};

const submitReport = form => {
    const reason = form.querySelector("input[name=report-reason]:checked");
    if (!reason) {
        const field = form.querySelector("[data-field=report-reason]");
        field.classList.add("field-invalid");
        if (!field.querySelector(".field-error")) {
            field.insertAdjacentHTML("beforeend", `<span class="field-error" role="alert">${icon("circle-alert")}Choose what's wrong.</span>`);
        }
        form.querySelector("input[name=report-reason]").focus();
        return;
    }
    const who = messaging.who;
    report({ about: who, post: messaging.post.id, reason: reason.value, text: form.querySelector("#report-text").value });
    if (form.querySelector("[name=report-block]").checked) {
        confirmBlock();
    } else {
        messaging.view = "main";
        messaging.paint("close");
        toast("Report sent. Thanks for telling us.");
    }
    toast("The report is stored and emailed to the site admin.", { standIn: true });
};

const replyAsOtherSide = () => {
    const c = messaging.conversation();
    const other = otherIn(c, me());
    messaging.unreadFrom = c.messages.length;
    sendMessage(c, other, CANNED_REPLIES[c.messages.length % CANNED_REPLIES.length]);
    messaging.paint();
    messaging.changed();
};

document.addEventListener("click", event => {
    const contact = event.target.closest(".card-contact");
    if (contact) {
        contactClicked(contact.closest(".card").dataset.id);
        return;
    }
    const target = event.target.closest("[data-msg], [data-proto=reply]");
    if (messaging && messaging.menu && !event.target.closest(".menu-wrap")) {
        messaging.menu = false;
        messaging.paint();
    }
    if (!target || !messaging) return;
    if (target.dataset.proto === "reply") return replyAsOtherSide();
    const action = target.dataset.msg;
    if (action === "close") {
        messaging.close();
    } else if (action === "menu") {
        messaging.menu = !messaging.menu;
        messaging.paint();
        if (messaging.menu) document.querySelector(".menu-item").focus();
    } else if (action === "report" || action === "block") {
        messaging.view = action;
        messaging.menu = false;
        messaging.paint(action === "block" ? "cancel" : "first");
    } else if (action === "cancel") {
        messaging.view = "main";
        messaging.paint("close");
    } else if (action === "confirm-block") {
        confirmBlock();
    } else if (action === "copy") {
        if (navigator.clipboard) navigator.clipboard.writeText(target.dataset.value).catch(() => {});
        target.innerHTML = `${icon("check")}Copied`;
        setTimeout(() => { target.innerHTML = `${icon("copy")}Copy`; }, 2000);
    }
});

document.addEventListener("input", event => {
    if (event.target.id === "composer" && messaging) {
        messaging.draft = event.target.value;
        autosize(event.target);
    }
    if (event.target.name === "report-reason") {
        const field = event.target.closest("[data-field]");
        field.classList.remove("field-invalid");
        field.querySelector(".field-error")?.remove();
    }
});

document.addEventListener("submit", event => {
    const form = event.target.dataset.msgForm;
    if (!form || !messaging) return;
    event.preventDefault();
    if (form === "send") send();
    else submitReport(event.target);
});

// Enter sends and Shift+Enter starts a new line, as on Discord. On a phone
// Enter is a new line and Send sends.
document.addEventListener("keydown", event => {
    if (!messaging) return;
    if (event.key === "Enter" && event.target.id === "composer" && !event.shiftKey && !event.isComposing
        && !matchMedia("(max-width: 639px)").matches) {
        event.preventDefault();
        send();
    } else if (event.key === "Escape") {
        if (messaging.menu) {
            messaging.menu = false;
            messaging.paint();
            document.querySelector("[data-msg=menu]").focus();
        } else if (messaging.view !== "main") {
            messaging.view = "main";
            messaging.paint("close");
        } else if (messaging.close === closePanel) {
            messaging.close();
        }
    }
});
