import { expect, Page, test } from "@playwright/test";
import { expectSignedInAs, password, signIn, signOut, submitPasswordSignIn, unique } from "../accounts";
import { discordUser, fakeDiscord, signUpWithDiscord } from "../discord";
import { expectPage } from "../pages";

async function fillSignUp(page: Page, email: string, nickname: string, password_ = password) {
    await page.getByLabel("Email").fill(email);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password_);
    await page.getByRole("button", { name: "Create account" }).click();
}

test("signing up with a password returns the player to the page they came from", async ({ page }) => {
    const nickname = unique("P");
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");

    await fillSignUp(page, `${unique("signup")}@example.com`, nickname);

    await expectPage(page, "/games/valorant");
    await expectSignedInAs(page, nickname);
});

test("signing up says what is wrong with each field", async ({ page }) => {
    await page.goto("/signup");

    await fillSignUp(page, "not an address", "", "short");
    await expect(page.getByText("Enter your email address.")).toBeVisible();
    await expect(page.getByText("Choose a nickname.")).toBeVisible();
    await expect(page.getByText("Use at least 8 characters.")).toBeVisible();

    await fillSignUp(page, `${unique("spaced")}@example.com`, "has spaces");
    await expect(page.getByText("Use up to 40 letters, digits, dashes, underscores and dots, without spaces.")).toBeVisible();

    // Both are the seeded Valorant account's, the nickname in other letter case.
    await fillSignUp(page, "valorant@example.com", unique("P"));
    await expect(page.getByText("An account already uses this email. Sign in instead.")).toBeVisible();

    await fillSignUp(page, `${unique("taken")}@example.com`, "valoranttester");
    await expect(page.getByText("This nickname is taken. Please pick another one.")).toBeVisible();
    await expectPage(page, "/signup");
});

test("signing in with a password takes the email or the nickname", async ({ page }) => {
    for (const emailOrNickname of ["new@example.com", "newtester"]) {
        await submitPasswordSignIn(page, emailOrNickname);
        await expectPage(page, "/");
        await expectSignedInAs(page, "NewTester");
        await signOut(page);
    }
});

test("signing in with a password says which half is wrong", async ({ page }) => {
    await submitPasswordSignIn(page, "NewTester", "wrong-password");
    await expect(page.getByText("Entered password is incorrect.")).toBeVisible();

    await submitPasswordSignIn(page, unique("nobody"));
    await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
});

test("signing in from the header returns the player to the page they came from", async ({ page }) => {
    await page.goto("/games/apex-legends");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByLabel("Email or nickname").fill("apex-legends@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();

    await expectPage(page, "/games/apex-legends");
    await expectSignedInAs(page, "ApexLegendsTester");
});

// The cookie of a signed-out session, which a browser also keeps across a reset of the
// database, names a session the server refuses. The header asks the server and shows
// Sign in only once it has answered.
test("a browser holding a session the server refuses can sign in again", async ({ page, context }) => {
    await signIn(page, "NewTester");
    const refused = await context.cookies();
    await signOut(page);
    const openHolding = async (path: string) => {
        await context.addCookies(refused);
        await page.goto(path);
        await expect(page.getByRole("banner").getByRole("link", { name: "Sign in" })).toBeVisible();
    };

    await openHolding("/games/valorant");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByLabel("Email or nickname").fill("new@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expectPage(page, "/games/valorant");
    await expectSignedInAs(page, "NewTester");
    await signOut(page);

    await openHolding(`/signup?back=${encodeURIComponent("/games/valorant")}`);
    await expectPage(page, "/signup");
    await expect(page.getByRole("heading", { name: "Create your account" })).toBeVisible();
});

// A sign-in form can be open in one tab while another tab signs in. Sending it replaces
// the session the browser holds, which ends for good.
test("signing in on a page opened before another tab signed in replaces that session", async ({ page, context }) => {
    const earlier = await context.newPage();
    await earlier.goto("/signin");
    await expect(earlier.getByLabel("Email or nickname")).toBeVisible();
    await signIn(page, "ApexLegendsTester");
    const replaced = await context.cookies();

    await earlier.getByLabel("Email or nickname").fill("new@example.com");
    await earlier.getByLabel("Password").fill(password);
    await earlier.getByRole("button", { name: "Sign in", exact: true }).click();
    await expectPage(earlier, "/");
    await expectSignedInAs(earlier, "NewTester");
    await page.reload();
    await expectSignedInAs(page, "NewTester");

    await context.addCookies(replaced);
    await page.reload();
    await expect(page.getByRole("banner").getByRole("link", { name: "Sign in" })).toBeVisible();
});

// The seed gives ExpiredTester two sessions with known tokens, one last used eleven
// months ago and one thirteen (`stacks/test-seed/players.sql`).
test("a session lasts a year from its last use, and every page renews it", async ({ page, context, baseURL }) => {
    const hold = async (token: string) => {
        await context.clearCookies();
        await context.addCookies([{ name: "teamtavern-token", value: token, url: baseURL! }]);
    };

    await hold("11111111111111111111111111111111111111cd");
    await page.goto("/");
    await expect(page.getByRole("banner").getByRole("link", { name: "Sign in" })).toBeVisible();

    const recent = "11111111111111111111111111111111111111ab";
    await hold(recent);
    const asked = page.waitForResponse(response => response.url().endsWith("/api/me"));
    await page.goto("/");
    await expectSignedInAs(page, "ExpiredTester");
    expect(await (await asked).headerValue("set-cookie"))
        .toContain(`teamtavern-token=${recent}; Max-Age=${365 * 24 * 60 * 60};`);
});

test("signing up with Discord asks Discord for the email and comes back through the sign-in page", async ({ page, baseURL }) => {
    const discord = await fakeDiscord(page, discordUser(`${unique("discord")}@example.com`, true));
    const nickname = unique("D");
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Discord" }).click();

    await expect(page.getByLabel("Nickname")).toHaveValue(discord.user.username);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();

    await expectPage(page, "/games/valorant");
    await expectSignedInAs(page, nickname);
    const [authorize] = discord.authorizeRequests;
    expect(authorize.searchParams.get("scope")).toBe("identify email");
    expect(authorize.searchParams.get("redirect_uri")).toBe(`${baseURL}/signin`);
    expect(authorize.searchParams.get("state")).toMatch(/^[0-9a-f]{32}$/);
});

test("the nickname prompt says when the nickname is taken", async ({ page }) => {
    const discord = await fakeDiscord(page, discordUser(null, false));
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Discord" }).click();

    await page.getByLabel("Nickname").fill("ValorantTester");
    await page.getByRole("button", { name: "Continue" }).click();
    await expect(page.getByText("This nickname is taken. Please pick another one.")).toBeVisible();

    const nickname = unique("D");
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
    expect(discord.authorizeRequests).toHaveLength(1);
});

test("signing in with Discord to an account signs in without asking for a nickname", async ({ page }) => {
    const discord = await fakeDiscord(page, discordUser(`${unique("again")}@example.com`, true));
    const nickname = await signUpWithDiscord(page, discord);
    await signOut(page);

    await page.goto("/signin");
    await page.getByRole("button", { name: "Continue with Discord" }).click();

    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

// Discord hands back a token for the tab that asked; one arriving without the state the
// page sent is refused, so a link can't sign someone in as another player.
test("a Discord token without the page's state signs nobody in", async ({ page }) => {
    const user = discordUser(null, false);
    const token = encodeURIComponent(JSON.stringify({ ...user, discriminator: "0" }));

    await page.goto(`/signin#token_type=Bearer&access_token=${token}&state=forged`);

    await expect(page.getByRole("heading", { name: "Sign in" })).toBeVisible();
    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

test("a Discord player is not found by a password sign-in or a password reset", async ({ page }) => {
    const email = `${unique("discordonly")}@example.com`;
    const discord = await fakeDiscord(page, discordUser(email, true));
    const nickname = await signUpWithDiscord(page, discord);
    await signOut(page);

    for (const emailOrNickname of [nickname, email]) {
        await submitPasswordSignIn(page, emailOrNickname);
        await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
    }

    await page.goto("/forgot-password");
    await page.getByLabel("Email").fill(email);
    await page.getByRole("button", { name: "Send link" }).click();
    await expect(page.getByText("No account signs in with a password at this email.")).toBeVisible();
});

test("a password player asks for a reset link", async ({ page }) => {
    await page.goto("/signin");
    await page.getByRole("link", { name: "Forgot password?" }).click();
    await expectPage(page, "/forgot-password");
    await page.getByLabel("Email").fill("apex-legends@example.com");
    await page.getByRole("button", { name: "Send link" }).click();

    await expect(page.getByRole("heading", { name: "Check your email" })).toBeVisible();
    await expect(page.getByText("We sent a link to apex-legends@example.com.")).toBeVisible();
});

test("signing out lands on the home page signed out", async ({ page }) => {
    await signIn(page, "ValorantTester");
    await page.goto("/games/valorant");

    await signOut(page);

    await expect(page.getByRole("button", { name: "Account menu" })).toHaveCount(0);
    await page.reload();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

// A browser sends a cookie without `SameSite` along with a form another site posts here,
// and Chromium does so for two minutes after the cookie is set even by default. The
// browser reports such a cookie as `Lax` all the same, so the test reads what it was sent.
test("the session cookie goes only with requests the site starts", async ({ page }) => {
    const signingIn = page.waitForResponse(response =>
        response.url().endsWith("/api/sessions") && response.request().method() === "POST");
    await signIn(page, "NewTester");

    const setCookies = await (await signingIn).headerValues("set-cookie");
    expect(setCookies).toHaveLength(1);
    expect(setCookies[0]).toMatch(/^teamtavern-token=/);
    expect(setCookies[0]).toContain("; SameSite=Lax");
    expect(setCookies[0]).toContain("; HttpOnly");
});

// A form can post JSON by naming a field with all of it but the last value, which the
// form's `=` and the field's value finish. Another site's form is labelled as text, so
// the server refuses it however well the JSON inside it reads.
test("another site's form signs nobody up", async ({ page, baseURL }) => {
    const nickname = unique("Forged");
    const registration = JSON.stringify({
        password: { email: `${nickname.toLowerCase()}@example.com`, nickname, password },
    });
    const field = registration.slice(0, -1) + ',"pad":"';
    await page.route("http://attacker.test/", route => route.fulfill({
        contentType: "text/html",
        body: `<form method="post" enctype="text/plain" action="${baseURL}/api/players">`
            + `<input name='${field}' value='"}'></form>`
            + `<script>document.forms[0].submit()</script>`,
    }));

    await page.goto("http://attacker.test/", { waitUntil: "commit" });
    await page.waitForURL(`${baseURL}/api/players`);

    await submitPasswordSignIn(page, nickname);
    await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
});
