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
    await page.goto("/games/apex");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByLabel("Email or nickname").fill("apex@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();

    await expectPage(page, "/games/apex");
    await expectSignedInAs(page, "ApexTester");
});

// The cookies of a signed-out session, which a browser also keeps across a reset of the
// database, name a session the server refuses, and the server removes them when it does.
// Until then a request still carries them, so the test waits for that before acting.
test("a browser holding a session the server refuses can sign in again", async ({ page, context }) => {
    await signIn(page, "NewTester");
    const refused = await context.cookies();
    await signOut(page);
    const openHolding = async (path: string) => {
        await context.addCookies(refused);
        await page.goto(path);
        await expect.poll(async () => (await context.cookies())
            .filter(cookie => cookie.name.startsWith("teamtavern-"))).toEqual([]);
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
    await page.getByLabel("Email").fill("apex@example.com");
    await page.getByRole("button", { name: "Send link" }).click();

    await expect(page.getByRole("heading", { name: "Check your email" })).toBeVisible();
    await expect(page.getByText("We sent a link to apex@example.com.")).toBeVisible();
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
test("the session cookies go only with requests the site starts", async ({ page }) => {
    const signingIn = page.waitForResponse(response =>
        response.url().endsWith("/api/sessions") && response.request().method() === "POST");
    await signIn(page, "NewTester");

    const setCookies = await (await signingIn).headerValues("set-cookie");
    expect(setCookies.map(cookie => cookie.split("=")[0]).sort())
        .toEqual(["teamtavern-id", "teamtavern-nickname", "teamtavern-token"]);
    for (const cookie of setCookies) {
        expect(cookie).toContain("; SameSite=Lax");
    }
});
