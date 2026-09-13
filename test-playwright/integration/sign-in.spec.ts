import { APIRequestContext, expect, test } from "@playwright/test";
import { queryTestDatabase } from "../test-stack";

// Every test registers its own players, so names and addresses carry a suffix unique to
// the run and to the test: the suite shares one database, and a retry must not collide
// with what the failed attempt left behind.
let registrations = 0;
function unique(prefix: string): string {
    registrations += 1;
    return `${prefix}${Date.now().toString(36)}${registrations}`;
}

const password = "tester-password";

type DiscordUser = { id: string, email?: string | null, verified?: boolean | null };

// `Server/Infrastructure/FetchDiscordUser.purs` calls the discord service in the test
// stack, which takes the access token for the user it names; see `DiscordStub/Main.purs`.
function discordToken({ id, email, verified }: DiscordUser): string {
    return encodeURIComponent(JSON.stringify({ id, username: id, discriminator: "0", email, verified }));
}

function discordUser(email: string | null, verified: boolean): DiscordUser {
    return { id: unique(""), email, verified };
}

// Registering and signing in both set a session cookie on the context that sent them, and
// both refuse a context that is already signed in, so each player gets its own context.
let contexts: APIRequestContext[] = [];
let newPlayer: () => Promise<APIRequestContext>;

test.beforeEach(async ({ playwright, baseURL }) => {
    contexts = [];
    newPlayer = async () => {
        const context = await playwright.request.newContext({ baseURL, ignoreHTTPSErrors: true });
        contexts.push(context);
        return context;
    };
});

test.afterEach(async () => {
    await Promise.all(contexts.map(context => context.dispose()));
});

// The registration and session bodies are PureScript Variants, which Yoga.JSON encodes as
// an object with the one case as its key, and so are the bad request bodies that come back.
async function registerWithPassword(email: string, nickname = unique("P")) {
    const context = await newPlayer();
    const response = await context.post("/api/players", {
        data: { password: { email, nickname, password } },
    });
    expect(response.status()).toBe(204);
    return { context, nickname };
}

async function registerWithDiscord(user: DiscordUser, nickname = unique("D")) {
    const context = await newPlayer();
    const response = await context.post("/api/players", {
        data: { discord: { nickname, accessToken: discordToken(user) } },
    });
    expect(response.status()).toBe(204);
    return { context, nickname };
}

function signInWithDiscord(context: APIRequestContext, user: DiscordUser) {
    return context.post("/api/sessions", {
        data: { discord: { accessToken: discordToken(user) } },
    });
}

function signInWithPassword(context: APIRequestContext, emailOrNickname: string) {
    return context.post("/api/sessions", {
        data: { password: { emailOrNickname, password } },
    });
}

// The player's own view carries the contact email; anyone else's view leaves it out.
async function contactEmail(context: APIRequestContext, nickname: string): Promise<string | null> {
    const response = await context.get(`/api/players/${nickname}`, { params: { timezone: "UTC" } });
    expect(response.status()).toBe(200);
    return ((await response.json()) as { email?: string | null }).email ?? null;
}

test("the schema requires one sign-in identity and a unique email among password players", async () => {
    const constraint = await queryTestDatabase(
        "select pg_get_constraintdef(oid) from pg_constraint where conname = 'player_identity_check'");
    expect(constraint).toEqual(["CHECK ((num_nonnulls(password_hash, discord_id) = 1))"]);

    const index = await queryTestDatabase(
        "select indexdef from pg_indexes where indexname = 'player_lower_email_key'");
    expect(index).toEqual([
        "CREATE UNIQUE INDEX player_lower_email_key ON public.player USING btree (lower((email)::text))"
            + " WHERE (password_hash IS NOT NULL)",
    ]);

    const emailConstraint = await queryTestDatabase(
        "select conname from pg_constraint where conname = 'player_email_key'");
    expect(emailConstraint).toEqual([]);
});

test("a Discord sign-up with a verified email gets it as the contact email", async () => {
    const email = `${unique("verified")}@example.com`;

    const { context, nickname } = await registerWithDiscord(discordUser(email, true));

    expect(await contactEmail(context, nickname)).toBe(email);
});

test("a Discord sign-up with an unverified email gets no contact email", async () => {
    const { context, nickname } = await registerWithDiscord(discordUser(`${unique("unverified")}@example.com`, false));

    expect(await contactEmail(context, nickname)).toBeNull();
});

// Discord vouches for the address, not its shape, and the column holds 254 characters.
test("a Discord player whose verified email is not a usable address still signs up and in", async () => {
    const tooLong = `${"a".repeat(250)}@example.com`;
    for (const email of ["", tooLong]) {
        const user = discordUser(email, true);
        const { context, nickname } = await registerWithDiscord(user);
        expect(await contactEmail(context, nickname)).toBeNull();

        const signInContext = await newPlayer();
        expect((await signInWithDiscord(signInContext, user)).status()).toBe(204);
        expect(await contactEmail(signInContext, nickname)).toBeNull();
    }
});

test("a Discord player without a contact email gets the verified one on the next sign-in", async () => {
    const user = discordUser(null, false);
    const { nickname } = await registerWithDiscord(user);
    const email = `${unique("later")}@example.com`;

    const context = await newPlayer();
    expect((await signInWithDiscord(context, { ...user, email, verified: true })).status()).toBe(204);
    expect(await contactEmail(context, nickname)).toBe(email);

    // A contact email once filled is the player's to change, not Discord's.
    const secondContext = await newPlayer();
    const changedAtDiscord = { ...user, email: `${unique("changed")}@example.com`, verified: true };
    expect((await signInWithDiscord(secondContext, changedAtDiscord)).status()).toBe(204);
    expect(await contactEmail(secondContext, nickname)).toBe(email);
});

test("a Discord account and a password account sharing an address are two players who both sign in", async () => {
    const email = `${unique("shared")}@example.com`;
    const { nickname: passwordNickname } = await registerWithPassword(email);
    const user = discordUser(email, true);
    const { nickname: discordNickname } = await registerWithDiscord(user);
    expect(discordNickname).not.toBe(passwordNickname);

    const passwordContext = await newPlayer();
    expect((await signInWithPassword(passwordContext, email)).status()).toBe(204);
    expect(await contactEmail(passwordContext, passwordNickname)).toBe(email);

    const discordContext = await newPlayer();
    expect((await signInWithDiscord(discordContext, user)).status()).toBe(204);
    expect(await contactEmail(discordContext, discordNickname)).toBe(email);
});

test("a password sign-in does not find a Discord player", async () => {
    const email = `${unique("discordonly")}@example.com`;
    const { nickname } = await registerWithDiscord(discordUser(email, true));

    for (const emailOrNickname of [nickname, email]) {
        const response = await signInWithPassword(await newPlayer(), emailOrNickname);
        expect(response.status()).toBe(400);
        expect(await response.json()).toEqual({ unknownPlayer: {} });
    }
});

test("a password reset is not offered to a Discord player", async () => {
    const email = `${unique("noreset")}@example.com`;
    await registerWithDiscord(discordUser(email, true));

    const response = await (await newPlayer()).post("/api/forgot-password", { data: { email } });

    expect(response.status()).toBe(404);
});

test("a Discord player changes the contact email without a password, even to a password player's", async () => {
    const passwordPlayersEmail = `${unique("taken")}@example.com`;
    await registerWithPassword(passwordPlayersEmail);
    const { context, nickname } = await registerWithDiscord(discordUser(null, false));

    const response = await context.put(`/api/players/${nickname}/email`, {
        data: { email: passwordPlayersEmail },
    });

    expect(response.status()).toBe(204);
    expect(await contactEmail(context, nickname)).toBe(passwordPlayersEmail);
});

test("a password player changes the email only with the password and only to a free address", async () => {
    const takenEmail = `${unique("taken")}@example.com`;
    await registerWithPassword(takenEmail);
    const { context, nickname } = await registerWithPassword(`${unique("own")}@example.com`);
    const url = `/api/players/${nickname}/email`;

    const withoutPassword = await context.put(url, { data: { email: `${unique("new")}@example.com` } });
    expect(withoutPassword.status()).toBe(400);
    expect(await withoutPassword.json()).toEqual({ wrongPassword: {} });

    const toTaken = await context.put(url, { data: { email: takenEmail.toUpperCase(), password } });
    expect(toTaken.status()).toBe(400);
    expect(await toTaken.json()).toEqual({ emailTaken: {} });

    const newEmail = `${unique("new")}@example.com`;
    expect((await context.put(url, { data: { email: newEmail, password } })).status()).toBe(204);
    expect(await contactEmail(context, nickname)).toBe(newEmail);
});
