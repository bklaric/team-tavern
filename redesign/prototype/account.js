// The account page (brief 11.5), reached from the account menu in the header.
// Two sections: the facts and contacts every post of the player's shows, which
// are edited here once for all of them, and the parts of the account nobody
// else sees.

// The facts posts show. They are asked for once and live on the account (brief
// 6, step 3), so the post screen folds them away and this page owns them.
const FACT_FIELDS = [
    { key: "nickname", label: "Nickname", kind: "text", hint: "Shown on your posts and in your messages." },
    { key: "birthday", label: "Birthday", kind: "date", hint: "Your posts show your age, never your birthday." },
    { key: "location", label: "Location", kind: "select", options: locationOptions, placeholder: "Choose a country" },
    { key: "languages", label: "Languages", kind: "tokens", options: languageList },
    { key: "timezone", label: "Timezone", kind: "timezone", hint: "Your online hours are written in this timezone. Everyone else sees them in theirs." },
];

// The contacts posts offer. A game account belongs to the games played with it,
// so the page asks for every kind and says where each shows (fields.js).
const CONTACT_FIELDS = [
    { key: "discord", label: "Discord", kind: "text", placeholder: "Your Discord username", hint: "On all your posts." },
    ...Object.values(GAME_ACCOUNTS).map(a =>
        ({ key: `account:${a.key}`, label: a.label, kind: "text", placeholder: a.placeholder, hint: a.note })),
];

const ALL_FACT_FIELDS = FACT_FIELDS.concat(CONTACT_FIELDS);
const factFieldByKey = key => ALL_FACT_FIELDS.find(f => f.key === key);

const accountValue = key => key.startsWith("account:")
    ? (account.accounts || {})[key.slice("account:".length)]
    : account[key];

// The emails the account may turn off (brief 11.5). Nothing here sends one, so
// a switch says what it would do.
const EMAIL_KINDS = [
    { key: "matches", label: "Matches", note: "When a new post fits one of yours." },
    { key: "messages", label: "Messages", note: "When someone writes, once per conversation until you read it." },
    { key: "renewals", label: "Renewals", note: "Before one of your posts expires, with one click to renew it." },
];

// What the page is doing: editing the facts, changing the email or how the
// player signs in, or about to delete the account.
let edits = null;
let emailEdits = null;
let signInEdits = null;
let confirmingDelete = false;
let errors = {};

// An account signs in one way (brief 11.5): with an email and a password, or
// with Discord, which knows it by its Discord id alone. The email is where the
// site's emails go either way, and Discord stays a contact either way.
const signsInWithDiscord = () => (account.signIn || (account.email ? "password" : "discord")) === "discord";

// The username the account's Discord carries, stood in for: the presets give
// theirs as a contact, and anyone else's is made from their nickname.
const discordHandle = () => account.discord || (account.nickname || "player").toLowerCase().replace(/[^a-z0-9._]/g, "");

// The definition list both sections are built from (brief 14.5): a label, what
// the account holds, and where there is one, the way to change it.
const dataListHtml = rows => `<dl class="data-list">${rows.map(r => `<div class="data-row"${r.id ? ` id="${r.id}" tabindex="-1"` : ""}>
    <dt>${escapeHtml(r.label)}</dt>
    <dd class="data-value">${r.value || `<span class="muted">Not given</span>`}</dd>
    ${r.action ? `<dd class="data-action">${r.action}</dd>` : ""}
</div>`).join("")}</dl>`;

const changeButton = (action, label) =>
    `<button class="button button-text button-small" type="button" data-action="${action}">${escapeHtml(label)}</button>`;

// Shown on your posts.

const contactsText = () => CONTACT_FIELDS
    .filter(f => has(accountValue(f.key)))
    .map(f => `${f.label} ${accountValue(f.key)}`)
    .join(" · ");

const factRows = () => [
    { label: "Nickname", value: account.nickname },
    { label: "Birthday", value: account.birthday && factText({ kind: "date" }, account.birthday) },
    { label: "Location", value: account.location },
    { label: "Languages", value: (account.languages || []).join(", ") },
    { label: "Timezone", value: account.timezone && factText({ kind: "timezone" }, account.timezone) },
    { label: "Contacts", value: contactsText() },
].map(r => ({ ...r, value: r.value ? escapeHtml(r.value) : "" }));

// How many posts a change here reaches. A player without posts is told what it
// is for rather than counted at.
const factsLead = () => {
    const n = account.posts.length;
    if (!n) return "Every post you write shows these, as your account has them then. You only give them once.";
    return `Every post you have shows these, as your account has them now. Change one here and it changes on ${
        n === 1 ? "your post" : `all ${n} of your posts`} at once.`;
};

const editValue = f => (f.key === "timezone" ? edits[f.key] || VIEWER_TZ : edits[f.key]);

const factFieldHtml = f => fieldShellHtml(f, fieldControlHtml(f, editValue(f)), { error: errorHtml(errors[f.key]) });

const factsFormHtml = () => `<form class="form" data-form="facts" novalidate>
    <div class="form-section">${FACT_FIELDS.map(factFieldHtml).join("")}</div>
    <div class="form-section">
        <h2>Contacts</h2>
        <p class="field-hint">Every post offers messages on TeamTavern, so these are optional.</p>
        ${CONTACT_FIELDS.map(factFieldHtml).join("")}
    </div>
    <div class="account-actions">
        <button class="button button-primary" type="submit">${icon("check")}Save changes</button>
        <button class="button button-text" type="button" data-action="cancel-facts">Cancel</button>
    </div>
</form>`;

const factsHtml = () => `<section class="account-section" aria-labelledby="facts-title">
    <h2 id="facts-title">Shown on your posts</h2>
    <p class="account-lead">${escapeHtml(factsLead())}</p>
    ${edits ? factsFormHtml() : `${dataListHtml(factRows())}
        <div class="account-actions">
            <button class="button button-outline button-small" type="button" data-action="edit-facts">${icon("pencil")}Edit</button>
        </div>`}
</section>`;

// Only you see this.

const formField = (f, value) => fieldShellHtml(f, fieldControlHtml(f, value), { error: errorHtml(errors[f.key]) });
const emailField = value => formField({ key: "email", label: "Email", kind: "email", autocomplete: "email" }, value);

const formActions = cancel => `<div class="account-actions">
    <button class="button button-primary" type="submit">${icon("check")}Save</button>
    <button class="button button-text" type="button" data-action="${cancel}">Cancel</button>
</div>`;

// The address, and whether the site can use it yet (brief 11.5): until its link
// is clicked the link is all it gets, and without one nothing is emailed.
const emailValue = () => {
    if (emailEdits) return `<form class="form form-tight" data-form="email" novalidate>
        ${emailField(emailEdits.email)}
        <p class="field-hint">${signsInWithDiscord() ? "" : "You sign in with it too. "}A new address gets a link to confirm it before anything else is sent there.</p>
        ${formActions("cancel-email")}
    </form>`;
    const state = emailState(account);
    if (state === "none") return `<span class="muted">No address</span>
        <p class="field-hint">Matches, messages and renewals are emailed, so without one the site can't tell you about them.</p>`;
    if (state === "unconfirmed") return `${escapeHtml(account.email)}
        <p class="field-hint">Not confirmed yet. We sent it a link, and send it nothing else until the link is clicked.</p>
        <button class="button button-text button-small" type="button" data-action="send-again">Send again</button>`;
    return escapeHtml(account.email);
};

const signInValue = () => {
    if (signInEdits) return signInFormHtml();
    return signsInWithDiscord() ? `<span class="data-line">${icon("discord")}Discord</span>` : "Email and password";
};

const passwordField = (label, hint) =>
    formField({ key: "password", label, kind: "password", autocomplete: "new-password", hint }, signInEdits.password);

// A password account moves to Discord or picks a new password; a Discord account
// moves to a password, which signs in with the account's email, so one without
// an address gives one here.
const signInFormHtml = () => `<form class="form form-tight" data-form="sign-in" novalidate>
    ${signsInWithDiscord()
        ? `<p class="field-hint">A password takes Discord's place${account.email
            ? `, and you sign in with ${escapeHtml(account.email)}` : ""}. Discord stays on your posts as a contact.</p>
            ${account.email ? "" : emailField(signInEdits.email)}
            ${passwordField("Password", "At least 8 characters.")}`
        : `<div class="field">
            <button class="button button-outline" type="button" data-action="use-discord" style="width: 100%">${icon("discord")}Continue with Discord</button>
            <p class="field-hint">Discord takes your password's place. Your email stays as it is.</p>
        </div>
        <div class="rule">or</div>
        ${passwordField("New password", "At least 8 characters.")}`}
    ${formActions("cancel-sign-in")}
</form>`;

const emailsValue = () => {
    const settings = emailSettings();
    const held = { unconfirmed: "None of these is sent until your address is confirmed.", none: "None of these is sent without an address." }[emailState(account)];
    return `<div class="switches">${EMAIL_KINDS.map(k => `<span data-email="${k.key}">${
        fieldControlHtml({ key: `email-${k.key}`, kind: "switch", text: k.label, note: k.note }, settings[k.key])
    }</span>`).join("")}</div>${held ? `<p class="field-hint">${held}</p>` : ""}`;
};

// Blocked players are listed here rather than behind a button of their own: the
// list is a few names at most, and a post says to come here to undo a block
// (brief 10, 11.1).
const blockedValue = () => {
    const blocked = blockedByViewer();
    if (!blocked.length) return `<span class="muted">Nobody is blocked.</span>`;
    return `<ul class="person-rows">${blocked.map(who => `<li class="person-row">
        <span class="person-name">${escapeHtml(who)}</span>
        <button class="button button-text button-small" type="button" data-unblock="${escapeHtml(who)}">Unblock</button>
    </li>`).join("")}</ul>
    <p class="field-hint">Neither of you sees the other's posts or messages. Unblocking brings all of it back.</p>`;
};

const deleteText = () => {
    const posts = account.posts.length;
    const conversations = conversationsOfViewer().length;
    const counted = [
        posts && `${posts} ${posts === 1 ? "post" : "posts"}`,
        conversations && `${conversations} ${conversations === 1 ? "conversation" : "conversations"}`,
    ].filter(Boolean);
    if (!counted.length) return "There is nothing on it to lose, and this can't be undone.";
    return `Your ${counted.join(" and ")} go with it${conversations ? ", for the players you were talking to as well" : ""}. This can't be undone.`;
};

const deleteHtml = () => confirmingDelete
    ? `<div class="confirm" role="alertdialog" aria-labelledby="confirm-title" aria-describedby="confirm-text">
        <h3 id="confirm-title">Delete your account?</h3>
        <p id="confirm-text">${escapeHtml(deleteText())}</p>
        <div class="confirm-actions">
            <button class="button button-destructive" type="button" data-action="confirm-delete">${icon("trash-2")}Delete account</button>
            <button class="button button-text" type="button" data-action="cancel-delete">Keep it</button>
        </div>
    </div>`
    : `<div class="account-actions account-actions-end">
        <button class="button button-destructive button-small" type="button" data-action="delete-account">${icon("trash-2")}Delete account</button>
    </div>`;

const privateHtml = () => `<section class="account-section" aria-labelledby="private-title">
    <h2 id="private-title">Only you see this</h2>
    <p class="account-lead">None of it shows on your posts.</p>
    ${dataListHtml([
        {
            label: "Email",
            value: emailValue(),
            action: emailEdits ? "" : changeButton("change-email", account.email ? "Change" : "Add"),
            id: "email",
        },
        {
            label: "Sign-in",
            value: signInValue(),
            action: signInEdits ? "" : changeButton("change-sign-in", signsInWithDiscord() ? "Use a password" : "Change"),
        },
        { label: "Emails", value: emailsValue(), id: "emails" },
        { label: "Blocked", value: blockedValue(), id: "blocked" },
    ])}
    ${deleteHtml()}
</section>`;

const paint = () => {
    paintChrome();
    document.getElementById("app").innerHTML = `<div class="account">
        <h1>Account</h1>
        ${factsHtml()}
        ${privateHtml()}
    </div>`;
    document.title = "Account · TeamTavern";
};

// Editing.

const trimmed = value => {
    const text = (value ?? "").trim();
    return text || undefined;
};

const startEditing = () => {
    edits = Object.fromEntries(ALL_FACT_FIELDS.map(f => [f.key, structuredClone(accountValue(f.key))]));
    errors = {};
    paint();
    document.querySelector("[data-form=facts] .input").focus();
};

const validateFacts = () => {
    const found = {};
    if (!trimmed(edits.nickname)) found.nickname = "Choose a nickname.";
    if (edits.birthday && !ageAt(edits.birthday)) found.birthday = "Enter the day you were born.";
    return found;
};

// A post keeps a copy of the card it was published with (site.js), because the
// home page spans games and one game's data is loaded at a time. A player post
// shows its owner's location and languages, so the copy is corrected too and
// the change shows wherever the post does.
const restampCards = before => {
    const codes = languages => (languages || []).map(languageCode).join(", ");
    const swaps = [
        [before.location, account.location],
        [codes(before.languages), codes(account.languages)],
    ].filter(([from, to]) => from && to && from !== to);
    if (!swaps.length) return;
    account.posts.filter(p => p.type === "player" && p.card).forEach(p => {
        p.card.facts = p.card.facts.map(f => {
            const swap = swaps.find(([from]) => f.text === from);
            return swap ? { ...f, text: swap[1] } : f;
        });
    });
};

const savedText = () => {
    const n = account.posts.length;
    if (!n) return "Saved.";
    return n === 1 ? "Saved. Your post shows it." : `Saved. All ${n} of your posts show it.`;
};

const saveFacts = () => {
    errors = validateFacts();
    if (Object.keys(errors).length) {
        paint();
        document.querySelector(".field-invalid .input").focus();
        return;
    }
    const before = { location: account.location, languages: account.languages };
    // Players are known by their nickname here, so a new one is carried into
    // the conversations, notifications and blocks that name the old one.
    renamePerson(account.nickname, trimmed(edits.nickname));
    account.nickname = trimmed(edits.nickname);
    FACT_FIELDS.filter(f => f.key !== "nickname").forEach(f => { account[f.key] = edits[f.key]; });
    account.discord = trimmed(edits.discord);
    account.accounts = Object.fromEntries(CONTACT_FIELDS
        .filter(f => f.key.startsWith("account:") && trimmed(edits[f.key]))
        .map(f => [f.key.slice("account:".length), trimmed(edits[f.key])]));
    restampCards(before);
    saveAccount();
    edits = null;
    paint();
    document.querySelector("[data-action=edit-facts]").focus();
    toast(savedText());
};

const validEmail = email => email && /.+@.+\..+/.test(email);

const showErrors = () => {
    paint();
    document.querySelector(".field-invalid .input").focus();
};

// A new address waits for its link, and nothing else is sent to it until then.
const takeEmail = email => {
    account.email = email;
    account.emailConfirmed = false;
    toast(`The real site emails ${email} a link to confirm it, and sends it nothing else until it is clicked.`, { standIn: true });
};

const saveEmail = () => {
    const email = trimmed(emailEdits.email);
    errors = validEmail(email) ? {} : { email: "Enter your email address." };
    if (Object.keys(errors).length) return showErrors();
    const changed = email !== account.email;
    if (changed) takeEmail(email);
    saveAccount();
    emailEdits = null;
    paint();
    document.querySelector("[data-action=change-email]").focus();
    if (changed) toast("Your email is changed.");
};

const saveSignIn = () => {
    const email = account.email || trimmed(signInEdits.email);
    const password = signInEdits.password || "";
    errors = {};
    if (!validEmail(email)) errors.email = "Enter your email address.";
    if (password.length < 8) errors.password = "Use at least 8 characters.";
    if (Object.keys(errors).length) return showErrors();
    const wasDiscord = signsInWithDiscord();
    if (email !== account.email) takeEmail(email);
    account.signIn = "password";
    saveAccount();
    signInEdits = null;
    paint();
    document.querySelector("[data-action=change-sign-in]").focus();
    toast(wasDiscord ? "You sign in with your email and password now." : "Your password is changed.");
};

// Discord takes the password's place and leaves the email as it is (brief
// 11.5). Its username fills the Discord contact only when the posts carry none,
// since the player may have given another.
const useDiscord = () => {
    const filled = !account.discord;
    account.discord ||= discordHandle();
    account.signIn = "discord";
    saveAccount();
    signInEdits = null;
    errors = {};
    paint();
    document.querySelector("[data-action=change-sign-in]").focus();
    toast(filled ? `You sign in with Discord now, and your posts offer Discord ${account.discord} as a contact.` : "You sign in with Discord now.");
    toast("The real button leaves for Discord and comes back here. A Discord that already signs in to another account is refused.", { standIn: true });
};

const sendAgain = () => toast(`Sent again to ${account.email}.`, {
    standIn: true,
    action: "Click the link",
    onAction: () => {
        account.emailConfirmed = true;
        saveAccount();
        paint();
        document.getElementById("email").focus();
        toast("Your email is confirmed.");
    },
});

const HANDLERS = {
    "edit-facts": startEditing,
    "cancel-facts": () => {
        edits = null;
        errors = {};
        paint();
        document.querySelector("[data-action=edit-facts]").focus();
    },
    "change-email": () => {
        emailEdits = { email: account.email || "" };
        signInEdits = null;
        errors = {};
        paint();
        document.querySelector("[data-form=email] .input").focus();
    },
    "cancel-email": () => {
        emailEdits = null;
        errors = {};
        paint();
        document.querySelector("[data-action=change-email]").focus();
    },
    "send-again": sendAgain,
    "change-sign-in": () => {
        signInEdits = { email: "", password: "" };
        emailEdits = null;
        errors = {};
        paint();
        document.querySelector("[data-form=sign-in] .input, [data-form=sign-in] button").focus();
    },
    "use-discord": useDiscord,
    "cancel-sign-in": () => {
        signInEdits = null;
        errors = {};
        paint();
        document.querySelector("[data-action=change-sign-in]").focus();
    },
    "delete-account": () => {
        confirmingDelete = true;
        paint();
        document.querySelector("[data-action=cancel-delete]").focus();
    },
    "cancel-delete": () => {
        confirmingDelete = false;
        paint();
        document.querySelector("[data-action=delete-account]").focus();
    },
    "confirm-delete": () => {
        toastOnNextPage("Your account is deleted.");
        deleteViewerAccount();
        location.href = "home.html";
    },
};

// A token list is redrawn where it stands, so the select it was added from
// keeps the focus.
const repaintField = key => {
    const element = document.querySelector(`[data-form=facts] [data-field="${key}"]`);
    element.outerHTML = factFieldHtml(factFieldByKey(key));
    document.querySelector(`[data-form=facts] [data-field="${key}"] [data-add]`).focus();
};

document.addEventListener("input", event => {
    const element = event.target.closest("[data-form] [data-field]");
    if (!element || event.target.matches("[data-add]")) return;
    const key = element.dataset.field;
    const form = element.closest("[data-form]").dataset.form;
    if (form === "sign-in") signInEdits[key] = event.target.value;
    else if (form === "email") emailEdits[key] = event.target.value;
    else edits[key] = readControl(element, factFieldByKey(key));
});

document.addEventListener("change", event => {
    const element = event.target.closest("[data-email]");
    if (element) {
        const kind = element.dataset.email;
        setEmailSetting(kind, event.target.checked);
        const name = EMAIL_KINDS.find(k => k.key === kind).label.toLowerCase();
        toast(event.target.checked
            ? `The site would start emailing you about ${name} again.`
            : `The site would stop emailing you about ${name}.`, { standIn: true });
        return;
    }
    if (event.target.matches("[data-form=facts] [data-add]") && event.target.value) {
        const key = event.target.closest("[data-field]").dataset.field;
        edits[key] = (edits[key] || []).concat(event.target.value);
        repaintField(key);
    }
});

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) return;
    const data = target.dataset;
    if (data.remove) {
        const key = target.closest("[data-field]").dataset.field;
        edits[key] = edits[key].filter(v => v !== data.remove);
        repaintField(key);
    } else if (data.unblock) {
        const who = data.unblock;
        unblock(who);
        paint();
        document.getElementById("blocked").focus();
        toast(`${who} is unblocked. Their posts, and anything you wrote to each other, are back.`, {
            action: "Undo",
            onAction: () => {
                block(who);
                paint();
            },
        });
    } else if (data.action && HANDLERS[data.action]) {
        HANDLERS[data.action]();
    }
});

document.addEventListener("submit", event => {
    const form = event.target.dataset.form;
    if (!form) return;
    event.preventDefault();
    if (form === "facts") saveFacts();
    else if (form === "email") saveEmail();
    else saveSignIn();
});

// Signed out there is no account to show, so the page asks the player to sign
// in and comes back here (brief 6, step 4). A row of its own is linked to from
// outside: every email's unsubscribe link lands on the switches (11.5), and a
// blocked player's post sends its viewer to the blocked list (11.1).
if (!signedIn()) {
    location.replace(signUpHref("signin"));
} else {
    paint();
    const opened = ["emails", "blocked"].includes(location.hash.slice(1)) && document.getElementById(location.hash.slice(1));
    if (opened) {
        // The row is the fragment such a link names, and the browser takes the
        // focus there; putting it in the middle of the screen is the page's own.
        opened.scrollIntoView({ block: "center" });
        opened.focus();
        if (opened.id === "emails") {
            toast("An email's unsubscribe link lands here, on the switches it is about.", { standIn: true });
        }
    }
}
