import { expect, Page, test } from "@playwright/test";

// The suite shares one database, so every player a test creates carries a suffix unique to
// the run and to the test, and a retry does not collide with what a failed attempt left.
let suffixes = 0;
function unique(prefix: string): string {
    suffixes += 1;
    return `${prefix}${Date.now().toString(36)}${suffixes}`;
}

const password = "tester-password";

type DiscordUser = { id: string, email: string | null, verified: boolean };

function discordUser(email: string | null, verified: boolean): DiscordUser {
    return { id: unique(""), email, verified };
}

// The pages send the browser to Discord's authorize URL, and Discord sends it back to the
// page named in `redirect_uri` with an access token in the fragment. This plays Discord's
// part: it answers with a token for whichever user is set at the time, which the discord
// service in the test stack reads back as that user (see `DiscordStub/Main.purs`), and
// records each authorize URL so a test can check what the page asked for.
type FakeDiscord = { user: DiscordUser, authorizeRequests: URL[] };

async function fakeDiscord(page: Page, user: DiscordUser): Promise<FakeDiscord> {
    const discord: FakeDiscord = { user, authorizeRequests: [] };
    await page.route(url => url.hostname === "discord.com" && url.pathname === "/api/oauth2/authorize", route => {
        const authorize = new URL(route.request().url());
        discord.authorizeRequests.push(authorize);
        const { id, email, verified } = discord.user;
        const token = encodeURIComponent(JSON.stringify({ id, username: id, discriminator: "0", email, verified }));
        return route.fulfill({
            status: 302,
            headers: { location: `${authorize.searchParams.get("redirect_uri")}#token_type=Bearer&access_token=${token}` },
        });
    });
    return discord;
}

async function registerWithPassword(page: Page, email: string, nickname = unique("P")): Promise<string> {
    await page.goto("/register");
    await page.locator('input[name="email"]').fill(email);
    await page.locator('input[name="nickname"]').fill(nickname);
    await page.locator('input[name="password"]').fill(password);
    // Button names start with their icon's glyph, and one more button starts "Create account".
    await page.getByRole("button", { name: /Create account$/ }).click();
    await page.waitForURL(url => url.pathname === "/onboarding/start");
    return nickname;
}

// The first button switches the form to Discord and the second, in its place, submits it.
async function signUpWithDiscord(page: Page, nickname = unique("D")): Promise<string> {
    await page.goto("/register");
    await page.getByRole("button", { name: "Create account with Discord" }).click();
    await page.locator('input[name="nickname"]').fill(nickname);
    await page.getByRole("button", { name: "Create account with Discord" }).click();
    await page.waitForURL(url => url.pathname === "/onboarding/start");
    return nickname;
}

async function submitPasswordSignIn(page: Page, emailOrNickname: string) {
    await page.goto("/signin");
    await page.locator('input[name="emailOrNickname"]').fill(emailOrNickname);
    await page.locator('input[name="password"]').fill(password);
    await page.getByRole("button", { name: /Sign in$/ }).click();
}

async function signInWithDiscord(page: Page) {
    await page.goto("/signin");
    await page.getByRole("button", { name: "Sign in with Discord" }).click();
}

// The header links a signed-in player to their own page, which is what names them.
async function expectSignedInAs(page: Page, nickname: string) {
    await expect(page.getByRole("link", { name: "Account" })).toHaveAttribute("href", `/players/${nickname}`);
}

// Onboarding, where a sign-up lands, has no top bar, so this signs out from home, and
// signing out reloads the site there.
async function signOut(page: Page) {
    await page.goto("/");
    await page.getByRole("button", { name: "Sign out" }).click();
    await expect(page.getByRole("button", { name: "Sign out" })).toHaveCount(0);
}

// Change email opens with the player's contact email filled in, the one place a page shows it.
async function openChangeEmail(page: Page, nickname: string) {
    await page.goto(`/players/${nickname}`);
    await page.locator(".options-button-icon").click();
    await page.locator(".popover-item", { hasText: "Change email" }).click();
    await expect(page.locator('input[name="email"]')).toBeVisible();
}

// A changed email reloads the player page it was changed on, which closes the form.
async function expectEmailChanged(page: Page) {
    await expect(page.locator('input[name="email"]')).toHaveCount(0);
    await page.waitForLoadState();
}

async function expectContactEmail(page: Page, nickname: string, email: string) {
    await openChangeEmail(page, nickname);
    await expect(page.locator('input[name="email"]')).toHaveValue(email);
}

test("signing up with Discord asks Discord for the email and keeps the verified address", async ({ page, baseURL }) => {
    const email = `${unique("signup")}@example.com`;
    const discord = await fakeDiscord(page, discordUser(email, true));

    const nickname = await signUpWithDiscord(page);

    expect(discord.authorizeRequests.map(authorize => authorize.searchParams.get("scope"))).toEqual(["identify email"]);
    expect(discord.authorizeRequests[0].searchParams.get("redirect_uri")).toBe(`${baseURL}/register`);
    await expectContactEmail(page, nickname, email);
});

test("signing up with Discord with an unverified email leaves the contact email empty", async ({ page }) => {
    await fakeDiscord(page, discordUser(`${unique("unverified")}@example.com`, false));

    const nickname = await signUpWithDiscord(page);

    await expectContactEmail(page, nickname, "");
});

// Discord vouches for the address, not its shape, and the column holds 254 characters.
test("a Discord player whose verified email is not a usable address still signs up and in", async ({ page }) => {
    const discord = await fakeDiscord(page, discordUser(null, true));

    for (const email of ["", `${"a".repeat(250)}@example.com`]) {
        discord.user = discordUser(email, true);
        const nickname = await signUpWithDiscord(page);
        await expectContactEmail(page, nickname, "");
        await signOut(page);

        await signInWithDiscord(page);
        await expectSignedInAs(page, nickname);
        await signOut(page);
    }
});

test("signing in with Discord fills a missing contact email with the verified address", async ({ page, baseURL }) => {
    const discord = await fakeDiscord(page, discordUser(null, false));
    const nickname = await signUpWithDiscord(page);
    await signOut(page);
    const email = `${unique("signin")}@example.com`;
    discord.user = { ...discord.user, email, verified: true };

    await signInWithDiscord(page);

    await expectSignedInAs(page, nickname);
    const signInRequest = discord.authorizeRequests[1];
    expect(signInRequest.searchParams.get("scope")).toBe("identify email");
    expect(signInRequest.searchParams.get("redirect_uri")).toBe(`${baseURL}/signin`);
    await expectContactEmail(page, nickname, email);
});

test("signing in with Discord never replaces a contact email the player has", async ({ page }) => {
    const email = `${unique("first")}@example.com`;
    const discord = await fakeDiscord(page, discordUser(email, true));
    const nickname = await signUpWithDiscord(page);
    await signOut(page);
    discord.user = { ...discord.user, email: `${unique("changed")}@example.com` };

    await signInWithDiscord(page);

    await expectSignedInAs(page, nickname);
    await expectContactEmail(page, nickname, email);
});

test("a Discord account and a password account sharing an address are two players who both sign in", async ({ page }) => {
    const email = `${unique("shared")}@example.com`;
    await fakeDiscord(page, discordUser(email, true));
    const passwordNickname = await registerWithPassword(page, email);
    await signOut(page);
    const discordNickname = await signUpWithDiscord(page);
    await signOut(page);

    await submitPasswordSignIn(page, email);
    await expectSignedInAs(page, passwordNickname);
    await signOut(page);

    await signInWithDiscord(page);
    await expectSignedInAs(page, discordNickname);
});

// The server parses the body whatever the header says, so only the request itself shows
// whether the page named what it sent.
test("a password sign-in sends its body labelled as JSON", async ({ page }) => {
    const nickname = await registerWithPassword(page, `${unique("labelled")}@example.com`);
    await signOut(page);

    const signInRequest = page.waitForRequest(request =>
        request.method() === "POST" && new URL(request.url()).pathname === "/api/sessions");
    await submitPasswordSignIn(page, nickname);

    expect((await signInRequest).headers()["content-type"]).toBe("application/json");
    await expectSignedInAs(page, nickname);
});

test("a password sign-in does not find a Discord player", async ({ page }) => {
    const email = `${unique("discordonly")}@example.com`;
    await fakeDiscord(page, discordUser(email, true));
    const nickname = await signUpWithDiscord(page);
    await signOut(page);

    for (const emailOrNickname of [nickname, email]) {
        await submitPasswordSignIn(page, emailOrNickname);
        await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
    }
});

test("a password reset is not offered to a Discord player", async ({ page }) => {
    const email = `${unique("noreset")}@example.com`;
    await fakeDiscord(page, discordUser(email, true));
    await signUpWithDiscord(page);
    await signOut(page);

    await page.goto("/forgot-password");
    await page.locator("form input").fill(email);
    await page.getByRole("button", { name: "Send password reset email" }).click();

    await expect(page.getByText("No account exists with this email.")).toBeVisible();
});

test("a Discord player changes the contact email without a password, even to a password player's", async ({ page }) => {
    const passwordPlayersEmail = `${unique("taken")}@example.com`;
    await registerWithPassword(page, passwordPlayersEmail);
    await signOut(page);
    await fakeDiscord(page, discordUser(null, false));
    const nickname = await signUpWithDiscord(page);

    await openChangeEmail(page, nickname);
    await expect(page.locator('input[name="password"]')).toHaveCount(0);
    await page.locator('input[name="email"]').fill(passwordPlayersEmail);
    await page.getByRole("button", { name: "Change email" }).click();

    await expectEmailChanged(page);
    await expectContactEmail(page, nickname, passwordPlayersEmail);
});

test("a password player changes the email only with the password and only to a free address", async ({ page }) => {
    const takenEmail = `${unique("taken")}@example.com`;
    await registerWithPassword(page, takenEmail);
    await signOut(page);
    const nickname = await registerWithPassword(page, `${unique("own")}@example.com`);
    const newEmail = `${unique("new")}@example.com`;
    await openChangeEmail(page, nickname);
    const emailInput = page.locator('input[name="email"]');
    const passwordInput = page.locator('input[name="password"]');
    const submit = page.getByRole("button", { name: "Change email" });

    await emailInput.fill(newEmail);
    await submit.click();
    await expect(page.getByText("Entered password is incorrect.")).toBeVisible();

    await emailInput.fill(takenEmail.toUpperCase());
    await passwordInput.fill(password);
    await submit.click();
    await expect(page.getByText("This email is already taken, please pick another one.")).toBeVisible();

    await emailInput.fill(newEmail);
    await submit.click();
    await expectEmailChanged(page);
    await expectContactEmail(page, nickname, newEmail);
});
