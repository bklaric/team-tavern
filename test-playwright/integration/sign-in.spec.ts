import { expect, Page, test } from "@playwright/test";
import { expectSignedInAs, password, signIn, signOut, submitPasswordSignIn, unique } from "../accounts";
import { discordUser, fakeDiscord, signUpWithDiscord } from "../discord";
import { fakeGoogle, googleUser, signUpWithGoogle } from "../google";
import { emails, openMail } from "../mail";
import { expectPage } from "../pages";
import { fakeSteam, signUpWithSteam, startSteamTrip, steamAnswer, steamId, steamNickname } from "../steam";

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

test("turning back at Discord leaves the player on the sign-in page they started from", async ({ page }) => {
    const discord = await fakeDiscord(page, discordUser(null, false));
    discord.cancel = true;
    await page.goto("/games/apex-legends");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByRole("button", { name: "Continue with Discord" }).click();

    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page).toHaveURL(/\/signin\?back=%2Fgames%2Fapex-legends$/);
    await page.getByLabel("Email or nickname").fill("apex-legends@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expectPage(page, "/games/apex-legends");
});

test("turning back at Discord leaves the player on the sign-up page they started from", async ({ page }) => {
    const discord = await fakeDiscord(page, discordUser(null, false));
    discord.cancel = true;
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Discord" }).click();

    await expectPage(page, "/signup");
    await expect(page.getByRole("heading", { name: "Create your account" })).toBeVisible();
    const nickname = unique("P");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await expectPage(page, "/games/valorant");
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

test("signing up with Steam asks for an email and comes back through the sign-in page", async ({ page, baseURL }) => {
    const steam = await fakeSteam(page);
    const nickname = unique("S");
    const address = `${nickname.toLowerCase()}@example.com`;
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Steam" }).click();

    await expect(page.getByLabel("Nickname")).toHaveValue(steamNickname(steam.steamId));
    await page.getByLabel("Nickname").fill("");
    await expect(page.getByText("We took it from Steam; change it if you like.")).toBeVisible();
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expect(page.getByText("Enter your email address.")).toBeVisible();
    await page.getByLabel("Email").fill(address);
    await page.getByRole("button", { name: "Continue" }).click();

    await expectPage(page, "/games/valorant");
    await expectSignedInAs(page, nickname);
    const [login] = steam.loginRequests;
    expect(login.searchParams.get("openid.mode")).toBe("checkid_setup");
    expect(login.searchParams.get("openid.realm")).toBe(baseURL);
    expect(login.searchParams.get("openid.return_to")).toMatch(new RegExp(`^${baseURL}/signin\\?steam=[0-9a-f]{32}$`));

    await openMail(page, address);
    await expect(emails(page, "Confirm your email")).toHaveCount(1);
});

test("the Steam nickname prompt says when the nickname is taken", async ({ page }) => {
    await fakeSteam(page);
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Steam" }).click();

    await page.getByLabel("Nickname").fill("ValorantTester");
    await page.getByLabel("Email").fill(`${unique("taken")}@example.com`);
    await page.getByRole("button", { name: "Continue" }).click();
    await expect(page.getByText("This nickname is taken. Please pick another one.")).toBeVisible();

    const nickname = unique("S");
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

test("signing in with Steam to an account signs in without asking for a nickname", async ({ page }) => {
    const steam = await fakeSteam(page);
    const nickname = await signUpWithSteam(page, steam);
    await signOut(page);

    await page.goto("/signin");
    await page.getByRole("button", { name: "Continue with Steam" }).click();

    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

test("turning back at Steam leaves the player on the sign-in page they started from", async ({ page }) => {
    const steam = await fakeSteam(page);
    steam.cancel = true;
    await page.goto("/games/apex-legends");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByRole("button", { name: "Continue with Steam" }).click();

    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page).toHaveURL(/\/signin\?back=%2Fgames%2Fapex-legends$/);
    await page.getByLabel("Email or nickname").fill("apex-legends@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expectPage(page, "/games/apex-legends");
});

test("turning back at Steam leaves the player on the sign-up page they started from", async ({ page }) => {
    const steam = await fakeSteam(page);
    steam.cancel = true;
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Steam" }).click();

    await expectPage(page, "/signup");
    await expect(page.getByRole("heading", { name: "Create your account" })).toBeVisible();
    const nickname = unique("P");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await expectPage(page, "/games/valorant");
});

// The address Steam returns to carries the state the page sent; an answer arriving without
// it is refused, so a link can't sign someone in as another player.
test("a Steam answer without the page's state signs nobody in", async ({ page, baseURL }) => {
    await page.goto(steamAnswer(`${baseURL}/signin?steam=forged`, steamId()));

    await expect(page.getByRole("heading", { name: "Sign in" })).toBeVisible();
    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

// Another site can get Steam's answer for a player's account, which names that site's page
// to return to. The server takes only an answer for this site's sign-in page.
test("a Steam answer for another site signs nobody in", async ({ page, baseURL }) => {
    const state = "0123456789abcdef0123456789abcdef";
    await page.goto("/signin");
    await startSteamTrip(page, state);

    await page.goto(steamAnswer(`${baseURL}/signin?steam=${state}`, steamId(), `http://attacker.test/signin?steam=${state}`));

    await expect(page.getByText("Steam couldn't sign you in. Continue with Steam again.")).toBeVisible();
    await expect(page.getByRole("button", { name: "Continue with Steam" })).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

// An answer counts only around the time Steam gave it, so one kept from long ago signs
// nobody in, though Steam has never been asked to check it.
test("a Steam answer from long ago signs nobody in", async ({ page, baseURL }) => {
    const state = "0123456789abcdef0123456789abcdef";
    await page.goto("/signin");
    await startSteamTrip(page, state);
    const anHourAgo = new Date(Date.now() - 60 * 60 * 1000);

    await page.goto(steamAnswer(`${baseURL}/signin?steam=${state}`, steamId(), undefined, anHourAgo));

    await expect(page.getByText("Steam couldn't sign you in. Continue with Steam again.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

// Each answer counts once, so one kept from the address bar or a log signs nobody in
// again. The sign-in page it lands on offers Steam again, which signs in.
test("a Steam answer signs in once", async ({ page }) => {
    const steam = await fakeSteam(page);
    const nickname = await signUpWithSteam(page, steam);
    await signOut(page);
    await page.goto("/signin");
    await page.getByRole("button", { name: "Continue with Steam" }).click();
    await expect(page.getByRole("button", { name: "Account menu" })).toBeVisible();
    await signOut(page);

    const answer = steam.answers[steam.answers.length - 1];
    await startSteamTrip(page, new URL(answer).searchParams.get("steam")!);
    await page.goto(answer);

    await expect(page.getByText("Steam couldn't sign you in. Continue with Steam again.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
    await page.getByRole("button", { name: "Continue with Steam" }).click();
    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

test("a Steam player is not found by a password sign-in", async ({ page }) => {
    const nickname = await signUpWithSteam(page, await fakeSteam(page));
    await signOut(page);

    for (const emailOrNickname of [nickname, `${nickname.toLowerCase()}@example.com`]) {
        await submitPasswordSignIn(page, emailOrNickname);
        await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
    }
});

test("signing up with Google takes its name and email and comes back through the sign-in page", async ({ page, baseURL }) => {
    const address = `${unique("google")}@example.com`;
    const google = await fakeGoogle(page, googleUser(address, true, "Zoë O'Brien"));
    const nickname = unique("G");
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expect(page.getByLabel("Nickname")).toHaveValue("ZoOBrien");
    await expect(page.getByText("We took it from Google; change it if you like.")).toBeVisible();
    await expect(page.getByLabel("Email")).toHaveCount(0);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();

    await expectPage(page, "/games/valorant");
    await expectSignedInAs(page, nickname);
    const [authorize] = google.authorizeRequests;
    expect(authorize.searchParams.get("response_type")).toBe("code");
    expect(authorize.searchParams.get("scope")).toBe("openid email profile");
    expect(authorize.searchParams.get("redirect_uri")).toBe(`${baseURL}/signin`);
    expect(authorize.searchParams.get("state")).toMatch(/^[0-9a-f]{32}$/);

    // Google verified the address, so it needs no confirming.
    await page.goto("/account");
    await expect(page.getByText(address)).toBeVisible();
    await expect(page.getByText("Not confirmed yet.")).toHaveCount(0);
});

test("signing up with Google with an address it hasn't verified emails a confirmation", async ({ page }) => {
    const address = `${unique("unverified")}@example.com`;
    await signUpWithGoogle(page, await fakeGoogle(page, googleUser(address, false)));

    await openMail(page, address);
    await expect(emails(page, "Confirm your email")).toHaveCount(1);
    await page.goto("/account");
    await expect(page.getByText("Not confirmed yet.")).toBeVisible();
});

test("the Google nickname prompt says when the nickname is taken", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(null, false, null));
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expect(page.getByLabel("Nickname")).toHaveValue("");
    await expect(page.getByText("We took it from Google")).toHaveCount(0);
    await page.getByLabel("Nickname").fill("ValorantTester");
    await page.getByRole("button", { name: "Continue" }).click();
    await expect(page.getByText("This nickname is taken. Please pick another one.")).toBeVisible();

    const nickname = unique("G");
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
    expect(google.authorizeRequests).toHaveLength(1);
});

test("signing in with Google to an account signs in without asking for a nickname", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(`${unique("again")}@example.com`, true));
    const nickname = await signUpWithGoogle(page, google);
    await signOut(page);

    await page.goto("/signin");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

test("turning back at Google leaves the player on the sign-in page they started from", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(null, false));
    google.cancel = true;
    await page.goto("/games/apex-legends");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page).toHaveURL(/\/signin\?back=%2Fgames%2Fapex-legends$/);
    await page.getByLabel("Email or nickname").fill("apex-legends@example.com");
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expectPage(page, "/games/apex-legends");
});

test("turning back at Google leaves the player on the sign-up page they started from", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(null, false));
    google.cancel = true;
    await page.goto("/games/valorant");
    await page.getByRole("link", { name: "Sign up" }).click();
    await expectPage(page, "/signup");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expectPage(page, "/signup");
    await expect(page.getByRole("heading", { name: "Create your account" })).toBeVisible();
    const nickname = unique("P");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await expectPage(page, "/games/valorant");
});

// Google and Discord both turn back with an error and the state in the address, so the
// trip that came back is the one whose state it is, not whichever is read first.
test("turning back at Google returns to Google's trip while a Discord trip is kept", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(null, false));
    google.cancel = true;
    await page.goto("/games/apex-legends");
    await page.getByRole("link", { name: "Sign in" }).click();
    await expectPage(page, "/signin");
    await page.evaluate(() => sessionStorage.setItem("tt-discord", JSON.stringify(
        { state: "0123456789abcdef0123456789abcdef", back: "/", from: "/signup", switching: false })));
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page).toHaveURL(/\/signin\?back=%2Fgames%2Fapex-legends$/);
});

// Google hands back a code for the tab that asked; one arriving without the state the page
// sent is refused, so a link can't sign someone in as another player.
test("a Google code without the page's state signs nobody in", async ({ page }) => {
    const { sub, email, verified: email_verified, name } = googleUser(null, false);
    const code = encodeURIComponent(JSON.stringify({ sub, email, email_verified, name }));

    await page.goto(`/signin?state=forged&code=${code}`);

    await expect(page.getByRole("heading", { name: "Sign in" })).toBeVisible();
    await expect(page.getByLabel("Email or nickname")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
});

// Google swaps each code once, so one kept from the address bar or a log signs nobody in
// again. The sign-in page it lands on offers Google again, which signs in.
test("a Google code signs in once", async ({ page }) => {
    const google = await fakeGoogle(page, googleUser(`${unique("once")}@example.com`, true));
    const nickname = await signUpWithGoogle(page, google);
    await signOut(page);

    google.replay = true;
    await page.goto("/signin");
    await page.getByRole("button", { name: "Continue with Google" }).click();

    await expect(page.getByText("Google couldn't sign you in. Continue with Google again.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
    google.replay = false;
    await page.getByRole("button", { name: "Continue with Google" }).click();
    await expectPage(page, "/");
    await expectSignedInAs(page, nickname);
});

test("a Google player is not found by a password sign-in or a password reset", async ({ page }) => {
    const email = `${unique("googleonly")}@example.com`;
    const nickname = await signUpWithGoogle(page, await fakeGoogle(page, googleUser(email, true)));
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
