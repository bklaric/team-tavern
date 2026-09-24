import { expect, Page, test } from "@playwright/test";
import { expectSignedInAs, password, signIn, signOut, signUp, submitPasswordSignIn, unique } from "../accounts";
import { discordUser, fakeDiscord, signUpWithDiscord } from "../discord";
import { body, emails, openMail } from "../mail";
import { expectPage } from "../pages";

// Every account this spec changes or deletes is one it signed up. Posts go into Rainbow Six
// Siege, whose feed no spec asserts whole. A renamed player writes to OverwatchTester and a
// blocking one blocks CounterStrike2Tester (`stacks/test-seed/players.sql`), whose inboxes and
// blocks no other spec reads.

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openFeed(page: Page, handle: string) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

// The value a row of the account's definition list shows.
const row = (page: Page, label: string) =>
    page.locator(".data-row").filter({ has: page.locator("dt", { hasText: new RegExp(`^${label}$`) }) })
        .locator(".data-value");

async function openAccount(page: Page) {
    await page.goto("/account");
    await expect(page.getByRole("heading", { name: "Shown on your posts" })).toBeVisible();
}

async function publishPlayerPost(page: Page) {
    await page.goto("/games/rainbow-six-siege/post/player");
    await expect(page.getByRole("heading", { name: "Tell groups and players about you" })).toBeVisible();
    await page.getByLabel("Location").selectOption("Croatia");
    await page.getByRole("button", { name: "Publish post" }).click();
    await expectPage(page, "/games/rainbow-six-siege/post/player/live");
}

test.describe("the account page", () => {
    // The feed leaves out the viewer's own posts, so a visitor reads the card.
    test("changes a fact on every post at once", async ({ page, browser }) => {
        const visitor = await (await browser.newContext()).newPage();
        const nickname = await signUp(page, "A");
        await publishPlayerPost(page);

        await openAccount(page);
        await expect(page.getByText("Change one here and it changes on your post at once.")).toBeVisible();
        await expect(row(page, "Location")).toHaveText("Croatia");
        await expect(row(page, "Birthday")).toHaveText("Not given");

        await page.getByRole("button", { name: "Edit" }).click();
        await expect(page.getByLabel("Nickname")).toBeFocused();
        await page.getByLabel("Location").selectOption("Germany");
        await page.getByLabel("Birthday").fill("2000-06-15");
        await page.getByRole("button", { name: "Save changes" }).click();

        await expect(page.locator(".toast-text")).toHaveText("Saved. Your post shows it.");
        await expect(row(page, "Location")).toHaveText("Germany");
        await expect(row(page, "Birthday")).toHaveText(/^15 June 2000, shown as age \d+$/);
        await expect(page.getByRole("button", { name: "Edit" })).toBeFocused();

        await openFeed(visitor, "rainbow-six-siege");
        await expect(card(visitor, nickname)).toContainText("Germany");
        await card(visitor, nickname).getByRole("link", { name: nickname, exact: true }).click();
        await expectPage(visitor, /^\/games\/rainbow-six-siege\/posts\/\d+$/);
        await expect(visitor.locator(".card")).toContainText("Germany");
    });

    test("renames the player where others see their messages, and refuses a taken name", async ({ browser }) => {
        const writer = await (await browser.newContext()).newPage();
        const owner = await (await browser.newContext()).newPage();

        const nickname = await signUp(writer, "R");
        await openFeed(writer, "overwatch");
        await card(writer, "OverwatchTester").locator(".card-contact").click();
        const panel = writer.getByRole("dialog", { name: "OverwatchTester" });
        await panel.getByRole("textbox", { name: "Message" }).fill("Duo tonight?");
        await panel.getByRole("button", { name: "Send" }).click();
        await expect(panel.locator(".message-own")).toHaveCount(1);

        await openAccount(writer);
        await writer.getByRole("button", { name: "Edit" }).click();
        await writer.getByLabel("Nickname").fill("");
        await writer.getByRole("button", { name: "Save changes" }).click();
        await expect(writer.getByText("Choose a nickname.")).toBeVisible();
        await expect(writer.getByLabel("Nickname")).toBeFocused();

        await writer.getByLabel("Nickname").fill("overwatchtester");
        await writer.getByRole("button", { name: "Save changes" }).click();
        await expect(writer.getByText("This nickname is taken. Please pick another one.")).toBeVisible();

        const renamed = unique("Renamed");
        await writer.getByLabel("Nickname").fill(renamed);
        await writer.getByRole("button", { name: "Save changes" }).click();
        await expect(row(writer, "Nickname")).toHaveText(renamed);
        await expectSignedInAs(writer, renamed);

        await signIn(owner, "overwatch@example.com");
        await owner.goto("/messages");
        await expect(owner.locator(".inbox-row").filter({ hasText: renamed })).toHaveCount(1);
        await expect(owner.locator(".inbox-row").filter({ hasText: nickname })).toHaveCount(0);
    });

    test("sends a signed out player to sign in and back to the email switches, which stay as set",
        async ({ page }) => {
        const nickname = await signUp(page, "S");
        await signOut(page);

        await page.goto("/account#emails");
        await expectPage(page, "/signin");
        await page.getByLabel("Email or nickname").fill(nickname);
        await page.getByLabel("Password").fill(password);
        await page.getByRole("button", { name: "Sign in", exact: true }).click();
        await expectPage(page, "/account");
        await expect(page.locator("#emails")).toBeFocused();

        // A new address waits for its link, and nothing is sent to it meanwhile.
        await expect(row(page, "Email")).toContainText("Not confirmed yet.");
        await expect(page.getByText("None of these is sent until your address is confirmed.")).toBeVisible();

        // A switch is flipped by its label, whose track covers the box.
        const messages = page.getByRole("checkbox", { name: /^Messages/ });
        await expect(messages).toBeChecked();
        const saved = page.waitForResponse(response => response.url().endsWith("/api/account/switches"));
        await page.locator("label.switch", { hasText: /^Messages/ }).click();
        await saved;
        await expect(messages).not.toBeChecked();
        await page.reload();
        await expect(page.getByRole("checkbox", { name: /^Messages/ })).not.toBeChecked();
        await expect(page.getByRole("checkbox", { name: /^Matches/ })).toBeChecked();
    });

    test("lists a blocked player where their post links, and unblocks them", async ({ page }) => {
        const owner = "CounterStrike2Tester";
        await signUp(page, "U");
        await openFeed(page, "counter-strike-2");
        const postPath = await card(page, owner).getByRole("link", { name: owner, exact: true }).getAttribute("href");
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });
        await panel.getByRole("button", { name: "More" }).click();
        await panel.getByRole("menuitem", { name: `Block ${owner}` }).click();
        await panel.getByRole("button", { name: `Block ${owner}` }).click();
        await expect(card(page, owner)).toHaveCount(0);

        await page.goto(postPath!);
        await page.getByRole("link", { name: "Unblock from your account" }).click();
        await expectPage(page, "/account");
        await expect(page.locator("#blocked")).toBeFocused();
        await expect(page.locator(".person-name")).toHaveText([owner]);

        await page.locator(".person-row").getByRole("button", { name: "Unblock" }).click();
        await expect(page.locator(".toast-text"))
            .toHaveText(`${owner} is unblocked. Their posts, and anything you wrote to each other, are back.`);
        await expect(row(page, "Blocked")).toHaveText("Nobody is blocked.");

        await page.locator(".toast").getByRole("button", { name: "Undo" }).click();
        await expect(page.locator(".person-name")).toHaveText([owner]);
        await page.locator(".person-row").getByRole("button", { name: "Unblock" }).click();
        await expect(row(page, "Blocked")).toHaveText("Nobody is blocked.");

        await openFeed(page, "counter-strike-2");
        await expect(card(page, owner)).toHaveCount(1);
    });

    test("deletes the account with its posts, after saying what goes", async ({ page }) => {
        const nickname = await signUp(page, "D");
        await publishPlayerPost(page);

        await openAccount(page);
        await page.getByRole("button", { name: "Delete account" }).click();
        const confirm = page.getByRole("alertdialog", { name: "Delete your account?" });
        await expect(confirm).toHaveAccessibleDescription("Your 1 post goes with it. This can't be undone.");
        await expect(confirm.getByRole("button", { name: "Keep it" })).toBeFocused();
        await confirm.getByRole("button", { name: "Keep it" }).click();
        await expect(page.getByRole("button", { name: "Delete account" })).toBeFocused();

        await page.getByRole("button", { name: "Delete account" }).click();
        await confirm.getByRole("button", { name: "Delete account" }).click();
        await expectPage(page, "/");
        await expect(page.locator(".toast-text")).toHaveText("Your account is deleted.");
        await expect(page.getByRole("banner").getByRole("link", { name: "Sign in" })).toBeVisible();

        await openFeed(page, "rainbow-six-siege");
        await expect(card(page, nickname)).toHaveCount(0);
        await submitPasswordSignIn(page, nickname);
        await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
    });

    test("changes the email, which waits for its own link while the old one confirms nothing", async ({ page }) => {
        const nickname = await signUp(page, "E");
        const old = `${nickname.toLowerCase()}@example.com`;
        const changed = `${nickname.toLowerCase()}.new@example.com`;
        const emailRow = page.locator("#email");

        await openAccount(page);
        await emailRow.getByRole("button", { name: "Change" }).click();
        await expect(page.getByLabel("Email", { exact: true })).toBeFocused();
        await page.getByLabel("Email", { exact: true }).fill("apex-legends@example.com");
        await emailRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(emailRow.getByText("Another account signs in with this email.")).toBeVisible();

        await page.getByLabel("Email", { exact: true }).fill(changed);
        await emailRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(page.locator(".toast-text")).toHaveText("Your email is changed.");
        await expect(row(page, "Email")).toContainText(changed);
        await expect(row(page, "Email")).toContainText("Not confirmed yet.");
        await expect(emailRow.getByRole("button", { name: "Change" })).toBeFocused();

        await emailRow.getByRole("button", { name: "Send again" }).click();
        await expect(page.locator(".toast-text")).toHaveText(`Sent again to ${changed}.`);

        await openMail(page, old);
        await body(emails(page, "Confirm your email")).getByRole("link", { name: "Confirm email" }).click();
        await expectPage(page, "/confirm-email");
        await expect(page.getByRole("heading", { name: "This link doesn't work" })).toBeVisible();

        await openMail(page, changed);
        await expect(emails(page, "Confirm your email")).toHaveCount(2);
        await body(emails(page, "Confirm your email").first()).getByRole("link", { name: "Confirm email" }).click();
        await expect(page.getByRole("heading", { name: "Your email is confirmed" })).toBeVisible();

        await openAccount(page);
        await expect(row(page, "Email")).toHaveText(changed);
        await expect(page.getByText("None of these is sent")).toHaveCount(0);
    });

    test("moves the sign-in to Discord and back to a password, each of which then signs in", async ({ page }) => {
        const nickname = await signUp(page, "M");
        const address = `${nickname.toLowerCase()}@example.com`;
        const discord = await fakeDiscord(page, discordUser(null, false));
        const signInRow = page.locator("#sign-in");

        await openAccount(page);
        await expect(row(page, "Sign-in")).toHaveText("Email and password");
        await signInRow.getByRole("button", { name: "Change" }).click();
        await signInRow.getByRole("button", { name: "Continue with Discord" }).click();
        await expectPage(page, "/account");
        await expect(page.locator(".toast-text")).toHaveText(
            `You sign in with Discord now, and your posts offer Discord ${discord.user.username} as a contact.`);
        await expect(row(page, "Sign-in")).toHaveText("Discord");
        await expect(signInRow.getByRole("button", { name: "Use a password" })).toBeFocused();

        await signOut(page);
        await submitPasswordSignIn(page, address);
        await expect(page.getByText("No account exists with this email or nickname.")).toBeVisible();
        await page.getByRole("button", { name: "Continue with Discord" }).click();
        await expectPage(page, "/");
        await expectSignedInAs(page, nickname);

        await openAccount(page);
        await signInRow.getByRole("button", { name: "Use a password" }).click();
        await expect(signInRow.getByText(
            `A password takes Discord's place, and you sign in with ${address}. Discord stays on your posts as a contact.`))
            .toBeVisible();
        await page.getByLabel("Password").fill("short");
        await signInRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(signInRow.getByText("Use at least 8 characters.")).toBeVisible();
        await page.getByLabel("Password").fill("a-new-password");
        await signInRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(page.locator(".toast-text")).toHaveText("You sign in with your email and password now.");
        await expect(row(page, "Sign-in")).toHaveText("Email and password");

        await signInRow.getByRole("button", { name: "Change" }).click();
        await page.getByLabel("New password").fill("another-password");
        await signInRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(page.locator(".toast-text")).toHaveText("Your password is changed.");

        await signOut(page);
        await submitPasswordSignIn(page, address, "another-password");
        await expectPage(page, "/");
        await expectSignedInAs(page, nickname);
    });

    test("refuses a Discord that signs in to another account", async ({ page, browser }) => {
        const other = await (await browser.newContext()).newPage();
        const user = discordUser(null, false);
        await signUpWithDiscord(other, await fakeDiscord(other, user));

        await signUp(page, "T");
        await fakeDiscord(page, user);
        const signInRow = page.locator("#sign-in");
        await openAccount(page);
        await signInRow.getByRole("button", { name: "Change" }).click();
        await signInRow.getByRole("button", { name: "Continue with Discord" }).click();
        await expectPage(page, "/account");
        await expect(signInRow.getByText("This Discord already signs in to another account.")).toBeVisible();
        await signInRow.getByRole("button", { name: "Cancel" }).click();
        await expect(row(page, "Sign-in")).toHaveText("Email and password");
    });

    test("asks a Discord account without an address for one to sign in with a password", async ({ page }) => {
        const nickname = await signUpWithDiscord(page, await fakeDiscord(page, discordUser(null, false)));
        const address = `${nickname.toLowerCase()}@example.com`;
        const signInRow = page.locator("#sign-in");

        await openAccount(page);
        await expect(row(page, "Email")).toContainText("No address");
        await expect(page.getByText("None of these is sent without an address.")).toBeVisible();
        await signInRow.getByRole("button", { name: "Use a password" }).click();
        await page.getByLabel("Password").fill("a-new-password");
        await signInRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(signInRow.getByText("Enter your email address.")).toBeVisible();

        await page.getByLabel("Email", { exact: true }).fill(address);
        await signInRow.getByRole("button", { name: "Save", exact: true }).click();
        await expect(page.locator(".toast-text")).toHaveText("You sign in with your email and password now.");
        await expect(row(page, "Email")).toContainText(address);
        await expect(row(page, "Email")).toContainText("Not confirmed yet.");

        await signOut(page);
        await submitPasswordSignIn(page, address, "a-new-password");
        await expectPage(page, "/");
        await openMail(page, address);
        await expect(emails(page, "Confirm your email")).toHaveCount(1);
    });
});
