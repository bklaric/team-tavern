// The post creation prototype (brief section 6): Type, Game, Post, Register or
// sign in, and Matches. Every step has its own URL, so Back steps back through
// the flow, and the draft waits in local storage between steps.

const query = new URLSearchParams(location.search);
const HANDLE = query.get("game");
const POST_TYPE = TYPES[query.get("type")] ? query.get("type") : null;
const HAS_DATA = typeof FEED_DATA !== "undefined";
const STEP = query.get("step") || (!POST_TYPE ? "type" : !HANDLE ? "game" : "post");
// A sign-up asked for from elsewhere, such as a contact panel, returns there
// (brief 6, step 4). TO names whom the player wanted to contact.
const NEXT = query.get("next");
const TO = query.get("to");

const url = changes => {
    const params = new URLSearchParams();
    Object.entries({ game: HANDLE, type: POST_TYPE, next: NEXT, to: TO, ...changes }).forEach(([k, v]) => { if (v) params.set(k, v); });
    return `post.html?${params}`;
};

// What the post screen says for each type.
const POST_TYPES = {
    player: {
        title: "Tell groups and players about you",
        words: "About you and what you're looking for",
        ideas: "How do you play? What are you aiming for? When are you usually on?",
        placeholder: "Peak Diamond, back after a break. Looking for a chill duo for ranked in the evenings.",
        days: 30,
    },
    group: {
        title: "Tell players about your group",
        words: "Tell people about your group",
        ideas: "How do you play? What are you aiming for this season?",
        placeholder: "Three friends who play most nights. We want to stop solo queuing for the last two spots.",
        days: 30,
    },
    community: {
        title: "Tell players about your community",
        words: "Tell people about your community",
        ideas: "What do members do together? How big are you, and what are the rules?",
        placeholder: "Weekly events, a friendly Discord and admins online most evenings. New members get a guide to get started.",
        days: 90,
    },
};
const PLURAL = { player: "players", group: "groups", community: "communities" };

const existing = () => account && account.posts.find(p => p.game === HANDLE && p.type === POST_TYPE);
const otherPosts = () => account ? account.posts.filter(p => p !== existing()) : [];

// Facts about the player and their contacts live on the account (game.js).
const accountFacts = () => ACCOUNT_FACTS[POST_TYPE];
const accountValue = key => account ? accountFactOf(account, key) : undefined;

const setAccountValue = (key, value) => {
    if (key === "gameAccount") account.accounts = { ...account.accounts, [EXTRAS.account.key]: value };
    else account[key] = value;
};

// The draft: one per game and type, keyed as the feed's description is, so a
// description prefills it field for field.

const DRAFT_KEY = `tt-draft-${HANDLE}-${POST_TYPE}`;
const saveDraft = () => localStorage.setItem(DRAFT_KEY, JSON.stringify(draft));

// What the account already knows fills in what the draft doesn't have: its
// facts and contacts, and for a group or community the languages and the region
// of the owner's location.
const withAccount = d => {
    const next = { ...d };
    accountFacts().forEach(key => {
        if (next[key] === undefined && accountValue(key) !== undefined) next[key] = structuredClone(accountValue(key));
    });
    if (POST_TYPE !== "player" && account) {
        if (next.languages === undefined && account.languages) next.languages = account.languages.slice();
        if (next.regions === undefined && account.location) next.regions = [REGION_OF[account.location]].filter(Boolean);
    }
    return next;
};

const withDefaults = d => ({
    reach: POST_TYPE === "community" ? undefined : "message",
    join: POST_TYPE === "community" ? "discord" : undefined,
    size: 2,
    wantedFrom: 1,
    wantedTo: 1,
    ...d,
});

const loadDraft = () => {
    let d = readJson(DRAFT_KEY, {});
    const from = query.get("from");
    // Edit, and Update post on a feed, start from the post as it is.
    if ((from === "edit" || from === "feed") && existing() && existing().draft) d = { ...structuredClone(existing().draft), editing: true };
    if (d.editing && !existing()) delete d.editing;
    if (from === "feed") {
        // A description's age isn't a birthday, so it stays behind. Updating a
        // post takes the description as it is, fields cleared from it included.
        const { age, ...described } = (readJson(`tt-description-${HANDLE}`, {}) || {})[POST_TYPE] || {};
        const cleared = d.editing ? Object.fromEntries(descriptionFields(POST_TYPE).map(f => [f.key, undefined])) : {};
        d = { ...d, ...cleared, ...described };
    }
    if (from) {
        query.delete("from");
        history.replaceState(null, "", `post.html?${query}`);
    }
    return withDefaults(withAccount(d));
};

let draft = HAS_DATA && POST_TYPE ? loadDraft() : {};
let errors = {};
let entered = {};
const changing = new Set();
let previewExpanded = false;
let confirmingDelete = false;
let overlay = query.get("sheet");
const previewMode = () => localStorage.getItem("tt-proto-preview") || "sheet";

// Fields. The screen has three parts: the post's fields, the player's words,
// and contact (brief 6, step 3).

const cardFields = () => {
    const languages = { key: "languages", label: "Languages", kind: "tokens", options: languageOptions };
    const regions = { key: "regions", label: "Regions", kind: "pills", options: regionOptions, hint: "Where the players you're looking for are" };
    const nameHint = signedIn()
        ? `Without one, the card says “${account.nickname}'s group”.`
        : "Without one, the card is named after you.";
    const fields = {
        player: [
            ...gameFields(),
            { key: "location", label: "Location", kind: "select", options: locationOptions, placeholder: "Choose a country", account: true },
            { ...languages, account: true },
            { key: "birthday", label: "Birthday", kind: "date", account: true, hint: "Your card shows your age, never your birthday." },
            { key: "mic", label: "Microphone", kind: "check", text: "I use a microphone" },
            ...timeFields(),
        ],
        group: [
            { key: "name", label: "Group name", kind: "text", hint: nameHint },
            { key: "size", label: "How many are you, and how many more do you want?", kind: "size" },
            ...gameFields(),
            regions,
            languages,
            { key: "mic", label: "Microphone", kind: "check", text: "Microphone required" },
            { key: "ageRange", label: "Ages", kind: "ageRange" },
            ...timeFields(),
        ],
        community: [
            { key: "name", label: "Community name", kind: "text", required: true },
            ...gameFields(),
            regions,
            languages,
            { key: "ageRange", label: "Ages", kind: "ageRange" },
            { key: "mic", label: "Microphone", kind: "check", text: "Microphone required" },
            ...timeFields(),
        ],
    };
    return fields[POST_TYPE];
};

const wordsField = () => ({
    key: "text",
    label: POST_TYPES[POST_TYPE].words,
    kind: "textarea",
    required: POST_TYPE === "community",
    placeholder: POST_TYPES[POST_TYPE].placeholder,
    hint: `Ideas: ${POST_TYPES[POST_TYPE].ideas}`,
});

const contactFields = () => POST_TYPE === "community"
    ? [
        {
            key: "join", label: "How do people join?", kind: "radioList",
            hint: "Players can always message you here too.",
            options: [
                { value: "discord", label: "They join our Discord server" },
                { value: "website", label: "They apply on our website" },
                { value: "message", label: "They message me first" },
            ],
        },
        { key: "discordServer", label: "Discord invite", kind: "text", placeholder: "discord.gg/…" },
        { key: "website", label: "Website", kind: "text", placeholder: "https://…" },
    ]
    : [
        {
            key: "reach", label: "How should people reach you?", kind: "radioList",
            hint: "People can always message you here too, so your contacts are optional.",
            options: [
                { value: "message", label: "Message me on TeamTavern" },
                { value: "offsite", label: "Add me on Discord or in game" },
                { value: "either", label: "Either is fine" },
            ],
        },
        { key: "discord", label: "Discord", kind: "discord", account: true, placeholder: "Your Discord username" },
        {
            key: "gameAccount", label: EXTRAS.account.label, kind: "text", account: true, placeholder: EXTRAS.account.placeholder,
            hint: POST_TYPE === "player" && EXTRAS.trackers
                ? `Your card links your ${EXTRAS.trackers.map(t => t.title).join(" and ")} profiles from it.`
                : undefined,
        },
        ...(POST_TYPE === "group" ? [
            { key: "discordServer", label: "Discord server", kind: "text", placeholder: "discord.gg/…" },
            { key: "website", label: "Website", kind: "text", placeholder: "https://…" },
        ] : []),
    ];

// The game's fields the post type is asked, in the game's order. A group asks
// for a range of every ordered field and for the slots it needs; a yes-or-no
// field is a checkbox, which a group reads as a need (game.js).
const gameFields = () => fieldsFor(POST_TYPE).map(f => {
    const key = answerKey(f, POST_TYPE);
    if (f.ilk === "boolean") {
        return { key, label: f.label, kind: "check", text: POST_TYPE === "player" ? `I can be ${article(f.label)} ${f.label.toLowerCase()}` : flagText(f, POST_TYPE) };
    }
    if (f.ordered) {
        return POST_TYPE === "player"
            ? { key, label: f.label, kind: "choose", options: optionsOf(f), placeholder: `Choose your ${f.label.toLowerCase()}` }
            : { key, label: `${f.label} range`, kind: "rankRange", options: optionsOf(f) };
    }
    return {
        key,
        label: POST_TYPE === "group" && f.slotted ? `${plural(f.label)} you need` : f.label,
        kind: f.ilk === "single" ? "radioPills" : "pills",
        options: optionsOf(f),
        all: f.slotted && anyText(f),
    };
});

const timeFields = () => [
    { key: "hours", label: "Usually online", kind: "hours" },
    { key: "timezone", label: "Timezone", kind: "timezone", account: true, hint: "Your hours are in this timezone. Everyone else sees them in theirs." },
];

const allFields = () => cardFields().concat(wordsField(), contactFields());
const fieldByKey = key => allFields().find(f => f.key === key);

// The timezone the hours are written in: the draft's, else the account's, else
// the browser's.
const valueOf = f => f.key === "timezone" ? draft.timezone || VIEWER_TZ : draft[f.key];

// The card a draft makes, as the feed would show it (game.js).
const postOf = (d, owner, updated) => draftPost(POST_TYPE, d, owner, updated);

// Until the player has registered, the preview reads "Posted by you".
const previewCard = () => renderCard({
    ...toCard(postOf(draft, signedIn() ? account.nickname : undefined), {}),
    preview: !signedIn(),
    expanded: previewExpanded,
});

const paintPreview = () => document.querySelectorAll("[data-preview]").forEach(el => { el.innerHTML = previewCard(); });

// Controls. The controls themselves are shared with the account page
// (fields.js); what is here is the post screen's own: the count steppers, and
// the Discord input, which signed out offers to sign up with it.

const stepper = (key, value, min, max, label) => `<div class="stepper" role="group" aria-label="${label}">
    <button type="button" data-step="${key}:-1" aria-label="Fewer"${value <= min ? " disabled" : ""}>${icon("minus")}</button>
    <output aria-live="polite">${value}</output>
    <button type="button" data-step="${key}:1" aria-label="More"${value >= max ? " disabled" : ""}>${icon("plus")}</button>
</div>`;

const controlHtml = (f, value) => {
    switch (f.kind) {
        case "discord":
            // Signed out, the Discord input offers to sign up with it (brief 6, step 3).
            return account ? inputHtml(f, value) : `<div class="input-row">${inputHtml(f, value)}
                <button class="button button-outline" type="button" data-action="discord-signup">${icon("discord")}Sign up with Discord</button></div>`;
        // How many you are and how many more you want, which is what the card
        // heading reads back (brief 5.2). The second number is there for a
        // group that will take either, and reads as one where they agree.
        case "size":
            return `<div class="count-row">${stepper("size", draft.size, 1, 30, "Players in the group")}
                <span class="muted">players, want</span>${stepper("wantedFrom", draft.wantedFrom, 1, 30, "At least")}
                <span class="muted">to</span>${stepper("wantedTo", draft.wantedTo, draft.wantedFrom, 30, "At most")}
                <span class="muted">more</span></div>`;
        // A rank range, an hours range and an age range are the description
        // bar's editors (game.js), so the bar and the screen ask alike.
        case "rankRange":
        case "hours":
        case "ageRange":
            return editor(f, value, "post");
        default:
            return fieldControlHtml(f, value);
    }
};

const readField = (element, f) => ["rankRange", "hours", "ageRange"].includes(f.kind)
    ? readEditor(element.querySelector("[data-editor]"), f)
    : readControl(element, f);

// An account fact the account holds shows as that value until the player
// changes it here. Changing one is changing it on every post they own.

const sameValue = (a, b) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null);

const folded = f => f.account && signedIn() && !changing.has(f.key)
    && has(accountValue(f.key)) && sameValue(draft[f.key], accountValue(f.key));

const accountFactHtml = (f, value) => `<div class="account-fact">
    <span class="account-fact-value">${escapeHtml(factText(f, value))}</span>
    <span class="account-fact-source">From your account</span>
    <button class="button button-text button-small" type="button" data-change="${f.key}">Change</button>
</div>`;

const noteHtml = f => {
    const changed = changing.has(f.key) || !sameValue(draft[f.key], accountValue(f.key));
    return f.account && signedIn() && otherPosts().length && changed
        ? `<span class="field-note">${icon("info")}Applies to all your posts</span>`
        : "";
};

const hintOf = f => account && !account.nickname && f.key === "discord"
    ? "From Discord. You'll pick a nickname when you publish."
    : f.hint;

const fieldHtml = f => {
    const value = valueOf(f);
    const fold = folded(f);
    return fieldShellHtml(f, fold ? accountFactHtml(f, value) : controlHtml(f, value), {
        labelled: !fold && LABELLED.includes(f.kind),
        hint: fold ? "" : hintOf(f),
        note: noteHtml(f),
        error: errorHtml(errors[f.key]),
    });
};

// The page around the steps.

// Signed out, Sign in on the post flow keeps the draft through it.
const pageHeaderOptions = () => (POST_TYPE && HANDLE && !NEXT ? { signInHref: url({ step: "register", mode: "signin" }) } : {});

const pageBarExtras = () => {
    const preview = previewMode();
    return STEP === "post" ? `<label>Preview below desktop <select data-proto="preview">
        <option value="sheet"${preview === "sheet" ? " selected" : ""}>On demand</option>
        <option value="top"${preview === "top" ? " selected" : ""}>Above the fields</option>
    </select></label>` : "";
};

const contextHtml = () => `<div class="step-context">
    <img src="${coverOf(HANDLE)}" alt="">
    <span><strong>${escapeHtml(gameTitle(HANDLE))}</strong> · ${TYPES[POST_TYPE].label} post</span>
    ${draft.editing ? "" : `<a class="button button-text button-small" href="${url({ game: null })}">Change game</a>
        <a class="button button-text button-small" href="${url({ type: null })}">Change type</a>`}
</div>`;

const missingHtml = () => `<div class="flow"><div class="missing-data">
    <h2>No sample for ${escapeHtml(gameTitle(HANDLE))}</h2>
    <p class="muted" style="margin-top: 8px">Run <code>./redesign/prototype/export-sample.sh ${escapeHtml(HANDLE)}</code> with the development stack's postgres running, or <a href="${url({ game: null })}">pick another game</a>.</p>
</div></div>`;

// Step 1: Type.

const renderType = () => {
    const mine = type => account && HANDLE && account.posts.some(p => p.game === HANDLE && p.type === type);
    return `<div class="flow">
        ${HANDLE ? `<div class="step-context"><img src="${coverOf(HANDLE)}" alt=""><strong>${escapeHtml(gameTitle(HANDLE))}</strong></div>` : ""}
        <h1>What are you posting?</h1>
        ${typeCardsHtml(type => url({ type }), type => mine(type) ? `You have one for ${escapeHtml(gameTitle(HANDLE))}` : "")}
        <p class="muted">Just looking? Open a game from Games to browse its feed.</p>
    </div>`;
};

// Step 2: Game.

const renderGame = () => `<div class="flow">
    <div class="step-context"><strong>${TYPES[POST_TYPE].label} post</strong>
        <a class="button button-text button-small" href="${url({ type: null })}">Change type</a></div>
    <h1>Which game?</h1>
    ${coverGridHtml(GAMES, {
        href: g => url({ game: g.handle }),
        mark: g => account && account.posts.some(p => p.game === g.handle && p.type === POST_TYPE) ? "Your post" : "",
    })}
</div>`;

// Step 3: Post, or, for a player who already has a post of this type for the
// game, that post with Edit it and Delete it.

const existingCard = post => post.draft
    ? renderCard({ ...toCard(postOf(post.draft, account.nickname, post.updated), {}), own: true, bare: true })
    : "";

const conversationsText = n =>
    n === 0 ? "It has no conversations." : `${n} ${n === 1 ? "conversation" : "conversations"} will be deleted for both of you.`;

const renderExisting = () => {
    const post = existing();
    const name = (post.draft && post.draft.name) || `your ${gameTitle(HANDLE)} ${post.type} post`;
    return `<div class="flow">
        ${contextHtml()}
        <h1>You already have a ${escapeHtml(gameTitle(HANDLE))} ${post.type} post</h1>
        <p class="flow-lead">You can have one ${post.type} post for each game. Edit this one, or delete it to start a new one.</p>
        ${existingCard(post)}
        ${confirmingDelete ? `<div class="confirm" role="alertdialog" aria-labelledby="confirm-title" aria-describedby="confirm-text">
            <h3 id="confirm-title">Delete ${escapeHtml(name)}?</h3>
            <p id="confirm-text">${conversationsText(conversationsOfPost(postId(account, post)).length)}</p>
            <div class="confirm-actions">
                <button class="button button-destructive" type="button" data-action="confirm-delete">${icon("trash-2")}Delete post</button>
                <button class="button button-text" type="button" data-action="cancel-delete">Keep it</button>
            </div>
        </div>` : `<div class="flow-actions">
            <button class="button button-primary" type="button" data-action="edit-existing">${icon("pencil")}Edit it</button>
            <button class="button button-destructive" type="button" data-action="delete-existing">${icon("trash-2")}Delete it</button>
        </div>`}
    </div>`;
};

const rulesHtml = () => {
    const days = POST_TYPES[POST_TYPE].days;
    if (draft.editing) return `<p>Saving renews your post: it stays active for ${days} days from today.</p>`;
    return `<p>Your post stays active for ${days} days. We'll email you before it expires, and tell you when someone new fits.</p>
        ${signedIn() ? "" : "<p>You'll create an account next. Nothing you've written is lost.</p>"}`;
};

const publishLabel = () => (draft.editing ? "Save post" : "Publish post");

const renderPost = () => {
    if (signedIn() && existing() && !draft.editing) return renderExisting();
    return `<div class="flow flow-wide">
        ${contextHtml()}
        <div class="post-layout${previewMode() === "top" ? " preview-top" : ""}">
            <form class="form" data-form="post" novalidate>
                <h1>${draft.editing ? `Edit your ${POST_TYPE} post` : POST_TYPES[POST_TYPE].title}</h1>
                <div class="form-section">${cardFields().map(fieldHtml).join("")}</div>
                <div class="form-section">${fieldHtml(wordsField())}</div>
                <div class="form-section">${contactFields().map(fieldHtml).join("")}</div>
                <div class="publish-footer">
                    <div>${rulesHtml()}</div>
                    <button class="button button-primary" type="submit">${publishLabel()}</button>
                </div>
            </form>
            <aside class="preview-column" aria-label="Preview">
                <p class="preview-label"><span>Preview</span><span>As it shows in the ${escapeHtml(GAME.title)} feed</span></p>
                <div data-preview>${previewCard()}</div>
            </aside>
        </div>
    </div>
    <div class="action-bar">
        ${previewMode() === "sheet" ? `<button class="button button-outline" type="button" data-action="open-preview">${icon("eye")}Preview</button>` : ""}
        <button class="button button-primary" type="button" data-action="publish">${publishLabel()}</button>
    </div>`;
};

// Step 4: Register or sign in, signed out only. The draft waits through it.

const regField = (name, label, type, autocomplete, hint) => `<div class="field${errors[name] ? " field-invalid" : ""}">
    <label class="field-label" for="r-${name}">${label}</label>
    <input class="input" id="r-${name}" name="${name}" type="${type}" value="${escapeHtml(entered[name] ?? "")}" autocomplete="${autocomplete}">
    ${hint ? `<span class="field-hint">${hint}</span>` : ""}
    ${errorHtml(errors[name])}
</div>`;

const registerMode = () => account && !account.nickname ? "nickname" : query.get("mode") || "signup";

// Where the player goes once signed in: back where they came from, or on with
// the post.
const leave = () => {
    location.href = NEXT;
};

const renderRegister = () => {
    if (signedIn()) {
        location.replace(NEXT || url({ step: null }));
        return "";
    }
    const mode = registerMode();
    if (NEXT) return renderSignUp(mode);
    const post = `${gameTitle(HANDLE)} ${POST_TYPE} post`;
    const discord = `<button class="button button-outline" type="button" data-action="discord-signup" style="width: 100%">${icon("discord")}Continue with Discord</button>
        <div class="rule">or</div>`;
    if (mode === "nickname") {
        entered.nickname ??= account.discordName;
        return `<div class="flow flow-narrow">
            <h1>Pick a nickname</h1>
            <p class="flow-lead">It's shown on your posts. We took it from Discord; change it if you like.</p>
            <form class="form form-tight" data-form="nickname" novalidate>
                ${regField("nickname", "Nickname", "text", "nickname")}
                <button class="button button-primary" type="submit">Publish post</button>
            </form>
        </div>`;
    }
    if (mode === "signin") {
        entered.email ??= "kestrel@example.com";
        return `<div class="flow flow-narrow">
            <h1>Sign in to publish</h1>
            <p class="flow-lead">Your ${escapeHtml(post)} goes live as soon as you're signed in. Nothing you wrote is lost.</p>
            ${discord}
            <form class="form form-tight" data-form="signin" novalidate>
                ${regField("email", "Email", "email", "email")}
                ${regField("password", "Password", "password", "current-password")}
                <button class="button button-primary" type="submit">Sign in and publish</button>
            </form>
            <p class="muted">New here? <a href="${url({ step: "register" })}">Create an account</a></p>
        </div>`;
    }
    return `<div class="flow flow-narrow">
        <h1>Create your account</h1>
        <p class="flow-lead">Your ${escapeHtml(post)} goes live as soon as you're signed up. Nothing you wrote is lost.</p>
        ${discord}
        <form class="form form-tight" data-form="signup" novalidate>
            ${regField("email", "Email", "email", "email")}
            ${regField("nickname", "Nickname", "text", "nickname", "Shown on your posts.")}
            ${regField("password", "Password", "password", "new-password", "At least 8 characters.")}
            <button class="button button-primary" type="submit">Create account and publish</button>
        </form>
        <p class="muted">Already have an account? <a href="${url({ step: "register", mode: "signin" })}">Sign in</a></p>
    </div>`;
};

// The same screen reached from anywhere else. From a contact panel it says whom
// the player is about to contact.
const renderSignUp = mode => {
    const discord = `<button class="button button-outline" type="button" data-action="discord-signup" style="width: 100%">${icon("discord")}Continue with Discord</button>
        <div class="rule">or</div>`;
    const lead = TO
        ? `Messages and contacts need an account. You'll come straight back to ${escapeHtml(TO)}'s post.`
        : "Find players, groups and communities, and hear when someone new fits.";
    if (mode === "nickname") {
        entered.nickname ??= account.discordName;
        return `<div class="flow flow-narrow">
            <h1>Pick a nickname</h1>
            <p class="flow-lead">It's shown on your posts and your messages. We took it from Discord; change it if you like.</p>
            <form class="form form-tight" data-form="nickname" novalidate>
                ${regField("nickname", "Nickname", "text", "nickname")}
                <button class="button button-primary" type="submit">Continue</button>
            </form>
        </div>`;
    }
    if (mode === "signin") {
        entered.email ??= "kestrel@example.com";
        return `<div class="flow flow-narrow">
            <h1>${TO ? `Sign in to contact ${escapeHtml(TO)}` : "Sign in"}</h1>
            ${TO ? `<p class="flow-lead">You'll come straight back to ${escapeHtml(TO)}'s post.</p>` : ""}
            ${discord}
            <form class="form form-tight" data-form="signin" novalidate>
                ${regField("email", "Email", "email", "email")}
                ${regField("password", "Password", "password", "current-password")}
                <button class="button button-primary" type="submit">Sign in</button>
            </form>
            <p class="muted">New here? <a href="${url({ step: "register", mode: null })}">Create an account</a></p>
        </div>`;
    }
    return `<div class="flow flow-narrow">
        <h1>${TO ? `Sign up to contact ${escapeHtml(TO)}` : "Create your account"}</h1>
        <p class="flow-lead">${lead}</p>
        ${discord}
        <form class="form form-tight" data-form="signup" novalidate>
            ${regField("email", "Email", "email", "email")}
            ${regField("nickname", "Nickname", "text", "nickname", "Shown on your posts and your messages.")}
            ${regField("password", "Password", "password", "new-password", "At least 8 characters.")}
            <button class="button button-primary" type="submit">Create account</button>
        </form>
        <p class="muted">Already have an account? <a href="${url({ step: "register", mode: "signin" })}">Sign in</a></p>
    </div>`;
};

// Signing in to an account that already has a post of this type for the game:
// update that post with the draft, or keep it and discard the draft.

const renderConflict = () => {
    const post = existing();
    if (!post) {
        location.replace(url({ step: null }));
        return "";
    }
    return `<div class="flow">
        <h1>You already have a ${escapeHtml(gameTitle(HANDLE))} ${POST_TYPE} post</h1>
        <p class="flow-lead">Update it with what you just wrote, or keep it as it is and discard what you wrote.</p>
        <p class="caption">Your post</p>
        ${existingCard(post)}
        <p class="caption">What you just wrote</p>
        ${renderCard({ ...toCard(postOf(draft, account.nickname), {}), bare: true })}
        <div class="flow-actions">
            <button class="button button-primary" type="button" data-action="update-existing">Update my post</button>
            <button class="button button-outline" type="button" data-action="discard-draft">Keep my post as it is</button>
        </div>
    </div>`;
};

// Step 5: Matches. The feed's matching, with the new post as the description.

const descriptionOf = d => describedBy(POST_TYPE, d);

const missCount = m => (m.compared ? m.misses : Infinity);

const fitsSentence = fits => {
    if (POST_TYPE !== "player") {
        return `${fits.length} ${fits.length === 1 ? "player fits" : "players fit"} your ${POST_TYPE} right now`;
    }
    const words = ["group", "community", "player"]
        .map(type => [type, fits.filter(e => e.post.type === type).length])
        .filter(([, n]) => n)
        .map(([type, n]) => `${n} ${n === 1 ? type : PLURAL[type]}`);
    const list = words.length > 1 ? `${words.slice(0, -1).join(", ")} and ${words.at(-1)}` : words[0];
    return `${list} ${fits.length === 1 ? "fits" : "fit"} you right now`;
};

const renderMatches = () => {
    const post = existing();
    if (!post || !post.draft) {
        location.replace(url({ step: null }));
        return "";
    }
    const described = descriptionOf(post.draft);
    const types = POST_TYPE === "player" ? ["group", "community", "player"] : ["player"];
    const entries = POSTS
        .filter(p => types.includes(p.type) && !p.expired && !p.own && !hiddenFromViewer(p.owner))
        .map(p => ({ post: p, m: compare(p, POST_TYPE, described) }));
    const fits = entries.filter(e => e.m.compared && !e.m.misses);
    const byCloseness = (a, b) => (missCount(a.m) - missCount(b.m) || 0) || b.post.updated - a.post.updated;
    const shown = (fits.length ? fits : entries).slice().sort(byCloseness).slice(0, 3);
    return `<div class="flow">
        <div class="live-heading">${icon("party-popper")}<h1>${query.get("updated") ? "Your post is updated" : "Your post is live"}</h1></div>
        <h2>${fits.length ? fitsSentence(fits) : `Nobody fits your ${POST_TYPE === "player" ? "post" : POST_TYPE} yet`}</h2>
        ${fits.length ? "" : `<p class="flow-lead">${shown.length ? "These come closest. " : ""}We'll email you when someone fits.</p>`}
        ${shown.length ? `<div class="feed-stack">${shown.map(e => renderCard(withViewer(toCard(e.post, e.m), e.post), true)).join("")}</div>` : ""}
        <div class="flow-actions">
            <a class="button button-primary" href="feed.html?game=${HANDLE}" data-action="see-all">See all</a>
        </div>
    </div>`;
};

// Overlays: the preview below a desktop, and Discord, which the prototype
// stands in for.

const overlayHtml = () => {
    if (overlay === "preview" && STEP === "post") {
        return `<div class="backdrop" data-action="close-overlay"></div>
        <div class="overlay overlay-modal" role="dialog" aria-modal="true" aria-labelledby="overlay-title">
            <div class="overlay-header"><h2 id="overlay-title" style="font-size: 16px">Preview</h2>
                <button class="icon-button" type="button" data-action="close-overlay" aria-label="Close">${icon("x")}</button></div>
            <div class="overlay-body">
                <p class="field-hint" style="margin-bottom: -12px">As it shows in the ${escapeHtml(GAME.title)} feed</p>
                <div data-preview>${previewCard()}</div>
            </div>
            <div class="overlay-footer"><button class="button button-primary" type="button" data-action="publish">${publishLabel()}</button></div>
        </div>`;
    }
    if (overlay === "discord") {
        return `<div class="backdrop" data-action="close-overlay"></div>
        <div class="overlay overlay-modal stand-in" role="dialog" aria-modal="true" aria-labelledby="overlay-title">
            <div class="overlay-header"><h2 id="overlay-title" style="font-size: 16px">Discord, stood in for</h2>
                <button class="icon-button" type="button" data-action="close-overlay" aria-label="Close">${icon("x")}</button></div>
            <div class="overlay-body">
                <p class="muted">The real button leaves for Discord and comes back here with the draft kept. Who signs in?</p>
                <div class="field">
                    <label class="field-label" for="mira-address">Mira's address, as Discord sends it</label>
                    <select class="select" id="mira-address">
                        <option value="verified">mira@example.com, verified</option>
                        <option value="unverified">mira@example.com, not verified</option>
                        <option value="none">None</option>
                    </select>
                </div>
                <button class="button button-outline" type="button" data-action="discord-new">${icon("discord")}A new player, Mira</button>
                <button class="button button-outline" type="button" data-action="discord-existing">${icon("discord")}Kestrel, who has an account</button>
            </div>
        </div>`;
    }
    return "";
};

const paintOverlay = () => {
    document.getElementById("overlay").innerHTML = overlayHtml();
    document.body.style.overflow = overlay ? "hidden" : "";
    const close = document.querySelector("#overlay [data-action=close-overlay].icon-button");
    if (close) close.focus();
};

const TITLES = {
    type: "New post", game: "Which game?", post: "Your post", register: "Create your account",
    conflict: "You already have a post", matches: "Your post is live",
};

const onMessagingChange = () => paint();

const paint = () => {
    paintChrome();
    const needsGame = ["post", "conflict", "matches"].includes(STEP) || (STEP === "register" && !NEXT);
    const render = needsGame && !HAS_DATA ? missingHtml : {
        type: renderType, game: renderGame, post: renderPost,
        register: renderRegister, conflict: renderConflict, matches: renderMatches,
    }[STEP];
    document.getElementById("app").innerHTML = render();
    document.title = TITLES[STEP];
    paintOverlay();
};

// Changes.

const repaintField = (key, focus) => {
    const element = document.querySelector(`[data-form=post] [data-field="${key}"]`);
    element.outerHTML = fieldHtml(fieldByKey(key));
    saveDraft();
    paintPreview();
    const target = focus && document.querySelector(`[data-form=post] [data-field="${key}"] ${focus}`);
    if (target) target.focus();
};

const clearError = key => {
    if (!errors[key]) return;
    delete errors[key];
    const element = document.querySelector(`[data-field="${key}"]`);
    if (!element) return;
    element.classList.remove("field-invalid");
    element.querySelector("[data-error]").innerHTML = "";
};

// Editing a contact can settle an error on how people reach or join.
const CONTACT_KEYS = ["reach", "join", "discord", "gameAccount", "discordServer", "website"];

const changed = (element, f) => {
    saveDraft();
    clearError(f.key);
    if (CONTACT_KEYS.includes(f.key)) CONTACT_KEYS.forEach(clearError);
    element.querySelector("[data-note]").innerHTML = noteHtml(f);
    paintPreview();
};

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

const stepValue = (key, delta) => {
    draft[key] += delta;
    draft.size = clamp(draft.size, 1, 30);
    draft.wantedFrom = clamp(draft.wantedFrom, 1, 30);
    draft.wantedTo = clamp(draft.wantedTo, draft.wantedFrom, 30);
};

const validate = () => {
    const found = {};
    const given = key => has(typeof draft[key] === "string" ? draft[key].trim() : draft[key]);
    if (POST_TYPE === "community") {
        if (!given("name")) found.name = "Give your community a name.";
        if (!given("text")) found.text = "Tell players what your community is about.";
        if (draft.join === "discord" && !given("discordServer")) found.discordServer = "Add your invite, or choose another way to join.";
        if (draft.join === "website" && !given("website")) found.website = "Add your website, or choose another way to join.";
    } else if (draft.reach === "offsite" && !given("discord") && !given("gameAccount")) {
        found.reach = `Add your Discord or ${EXTRAS.account.label} below, or choose another way.`;
    }
    if (draft.hours && !draft.hours.from !== !draft.hours.to) found.hours = "Choose both times, or neither.";
    return found;
};

// Publishing writes the account facts and contacts back to the account, and
// stands the post in place of any the player had of this type for the game.
const publish = updated => {
    accountFacts().forEach(key => { if (has(draft[key])) setAccountValue(key, draft[key]); });
    if (!account.timezone) account.timezone = draft.timezone || VIEWER_TZ;
    const previous = existing();
    const { editing, ...fields } = draft;
    const stored = { game: HANDLE, type: POST_TYPE, updated: NOW.toISOString(), draft: fields };
    const post = postOf(fields, account.nickname, stored.updated);
    const card = toCard(post, {});
    account.posts = account.posts.filter(p => p !== previous).concat({ ...stored, card: { slots: card.slots, facts: card.facts } });
    saveAccount();
    // A post notifies the owners it fits when it is published, or renewed after
    // it expired; editing or renewing an active one notifies nobody (brief 8).
    if (!previous || expiredPost(previous)) {
        notifyOwnersFitBy({ ...post, id: postId(account, stored) }, POST_TYPE, account.nickname);
    }
    localStorage.removeItem(DRAFT_KEY);
    location.href = url({ step: "matches", updated: updated || editing ? "1" : null });
};

const submitPost = () => {
    errors = validate();
    if (Object.keys(errors).length) {
        overlay = null;
        paint();
        const first = document.querySelector(".field-invalid");
        first.scrollIntoView({ block: "center" });
        const control = first.querySelector("input, textarea, select");
        if (control) control.focus({ preventScroll: true });
        return;
    }
    if (!signedIn()) {
        location.href = url({ step: "register" });
        return;
    }
    publish(false);
};

// Signed in mid-flow: an existing post of this type for the game is a choice
// between the two. Otherwise, from the post screen the player goes on writing;
// from the register step the post goes live.
const afterSignIn = onPostScreen => {
    if (NEXT) return leave();
    draft = withDefaults(withAccount(draft));
    saveDraft();
    if (existing()) {
        location.href = url({ step: "conflict" });
    } else if (onPostScreen) {
        overlay = null;
        paint();
    } else {
        publish(false);
    }
};

const submitRegister = (mode, form) => {
    const value = name => (form.querySelector(`[name="${name}"]`) || { value: "" }).value.trim();
    entered = { email: value("email"), nickname: value("nickname") };
    errors = {};
    if (mode !== "nickname" && !/.+@.+\..+/.test(value("email"))) errors.email = "Enter your email address.";
    if (mode !== "signin" && !value("nickname")) errors.nickname = "Choose a nickname.";
    if (mode === "signup" && value("password").length < 8) errors.password = "Use at least 8 characters.";
    if (mode === "signin" && !value("password")) errors.password = "Enter your password.";
    if (Object.keys(errors).length) {
        paint();
        document.querySelector(".field-invalid input").focus();
        return;
    }
    if (mode === "signin") {
        useAccount("kestrel");
        afterSignIn(false);
        return;
    }
    if (mode === "signup") {
        account = { id: "new", nickname: value("nickname"), email: value("email"), emailConfirmed: false, accounts: {}, posts: [] };
        toastOnNextPage(`The real site emails ${account.email} a link to confirm it, and sends it nothing else until it is clicked.`, { standIn: true });
    } else {
        account.nickname = value("nickname");
    }
    saveAccount();
    if (NEXT) leave();
    else publish(false);
};

const HANDLERS = {
    "publish": submitPost,
    "open-preview": () => {
        overlay = "preview";
        paintOverlay();
    },
    "close-overlay": () => {
        overlay = null;
        paintOverlay();
    },
    "discord-signup": () => {
        overlay = "discord";
        paintOverlay();
    },
    "discord-new": () => {
        // Sign-up takes Discord's username for the contact and its address for
        // the email; an address Discord hasn't verified is confirmed the way a
        // typed one is (brief 6, step 4).
        const address = document.getElementById("mira-address").value;
        account = { id: "new", nickname: null, discordName: "Mira", discord: "mira.plays", signIn: "discord", accounts: {}, posts: [] };
        if (address !== "none") {
            account.email = "mira@example.com";
            account.emailConfirmed = address === "verified";
        }
        if (address === "unverified") {
            toastOnNextPage("Discord hasn't verified mira@example.com, so the real site emails it a link to confirm it, and nothing else until it is clicked.", { standIn: true });
        }
        saveAccount();
        if (POST_TYPE && HANDLE) {
            draft.discord = account.discord;
            saveDraft();
        }
        overlay = null;
        paint();
    },
    "discord-existing": () => {
        useAccount("kestrel");
        afterSignIn(STEP === "post");
    },
    "edit-existing": () => {
        draft = withDefaults(withAccount({ ...structuredClone(existing().draft), editing: true }));
        saveDraft();
        paint();
    },
    "delete-existing": () => {
        confirmingDelete = true;
        paint();
        document.querySelector("[data-action=cancel-delete]").focus();
    },
    "cancel-delete": () => {
        confirmingDelete = false;
        paint();
    },
    "confirm-delete": () => {
        deleteConversationsOf(postId(account, existing()));
        account.posts = account.posts.filter(p => p !== existing());
        saveAccount();
        localStorage.removeItem(DRAFT_KEY);
        draft = withDefaults(withAccount({}));
        confirmingDelete = false;
        paint();
    },
    "update-existing": () => publish(true),
    "discard-draft": () => {
        localStorage.removeItem(DRAFT_KEY);
        location.href = `feed.html?game=${HANDLE}`;
    },
    // The feed opens with the description taken from the post (brief 11.2).
    "see-all": () => describeFeed(HANDLE, POST_TYPE, descriptionOf(existing().draft)),
};

document.addEventListener("input", event => {
    if (event.target.closest("[data-proto]") || event.target.matches("[data-add]")) return;
    const element = event.target.closest("[data-form=post] [data-field]");
    if (!element) return;
    const f = fieldByKey(element.dataset.field);
    draft[f.key] = readField(element, f);
    changed(element, f);
});

document.addEventListener("change", event => {
    const target = event.target;
    if (target.matches("[data-add]") && target.value) {
        const key = target.closest("[data-field]").dataset.field;
        draft[key] = (draft[key] || []).concat(target.value);
        repaintField(key, "[data-add]");
        clearError(key);
    } else if (target.dataset.proto === "preview") {
        localStorage.setItem("tt-proto-preview", target.value);
        paint();
    }
});

document.addEventListener("click", event => {
    const target = event.target.closest("button, a");
    if (!target) return;
    const data = target.dataset;
    if (target.matches(".card-details-toggle") && target.closest("[data-preview]")) {
        previewExpanded = target.closest(".card").classList.contains("card-expanded");
    } else if (target.getAttribute("href") === "#") {
        event.preventDefault();
    } else if (data.remove) {
        const key = target.closest("[data-field]").dataset.field;
        draft[key] = draft[key].filter(v => v !== data.remove);
        repaintField(key, "[data-add]");
    } else if (data.step) {
        const [key, delta] = data.step.split(":");
        stepValue(key, Number(delta));
        repaintField(target.closest("[data-field]").dataset.field, `[data-step="${data.step}"]:not(:disabled)`);
    } else if (data.change) {
        changing.add(data.change);
        repaintField(data.change, "input, select");
    } else if (data.action && HANDLERS[data.action]) {
        HANDLERS[data.action](target, event);
    }
});

document.addEventListener("submit", event => {
    const form = event.target.dataset.form;
    if (!form) return;
    event.preventDefault();
    if (form === "post") submitPost();
    else submitRegister(form, event.target);
});

document.addEventListener("keydown", event => {
    if (event.key === "Escape" && overlay) {
        overlay = null;
        paintOverlay();
    }
});

paint();
