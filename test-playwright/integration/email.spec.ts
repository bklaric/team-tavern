import { expect, Locator, Page, test } from "@playwright/test";
import { signIn, signOut, signUp, submitPasswordSignIn } from "../accounts";
import { expectPage } from "../pages";

// The test stack's mail service keeps what the site sends, and shows an address's mail at
// /mail on the site's origin, newest first, each email in a frame whose links open in the
// tab. MailTester and QuietTester (`stacks/test-seed/players.sql`) are written to only
// here; the players writing to them are new to each test.

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openFeed(page: Page, handle: string) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

// The message is shown once the site has taken it, and the site has sent any email about
// it by then.
async function send(panel: Locator, message: string) {
    const box = panel.getByRole("textbox", { name: "Message" });
    await box.fill(message);
    await panel.getByRole("button", { name: "Send" }).click();
    await expect(panel.getByText(message)).toBeVisible();
    await expect(box).toHaveValue("");
}

async function writeTo(page: Page, handle: string, owner: string, messages: string[]) {
    await openFeed(page, handle);
    await card(page, owner).locator(".card-contact").click();
    const panel = page.getByRole("dialog", { name: owner });
    for (const message of messages) await send(panel, message);
}

async function openMail(page: Page, address: string) {
    await page.goto(`/mail?to=${encodeURIComponent(address)}`);
    await expect(page.getByRole("heading", { name: `Mail to ${address}` })).toBeVisible();
}

const emails = (page: Page, subject: string) =>
    page.getByRole("article").filter({ has: page.getByRole("heading", { name: subject, exact: true }) });

const body = (email: Locator) => email.frameLocator("iframe");

test.describe("email", () => {
    test("tells an owner of a conversation once until they read it, and opens it", async ({ page, browser }) => {
        const nickname = await signUp(page, "M");
        await writeTo(page, "counter-strike-2", "MailTester", ["Hi, want to play tonight?", "I'm on from nine."]);

        const owner = await (await browser.newContext()).newPage();
        await signIn(owner, "mail@example.com");
        await openMail(owner, "mail@example.com");
        const email = emails(owner, `${nickname} sent you a message on TeamTavern`);
        await expect(email).toHaveCount(1);
        await expect(body(email).getByText("Hi, want to play tonight?")).toBeVisible();
        await expect(body(email).getByText("I'm on from nine.")).toHaveCount(0);
        await expect(body(email).getByRole("link", { name: "Renew" })).toHaveCount(0);

        await body(email).getByRole("link", { name: "Reply" }).click();
        await expectPage(owner, /^\/messages\/\d+$/);
        await expect(owner.getByRole("region", { name: nickname }).locator(".message").first())
            .toHaveText(`${nickname}: Hi, want to play tonight?`);
    });

    test("sends nothing to an owner who switched message emails off", async ({ page }) => {
        await signUp(page, "M");
        await writeTo(page, "counter-strike-2", "QuietTester", ["Hi, want to play tonight?"]);

        await openMail(page, "quiet@example.com");
        await expect(page.getByText("No emails.")).toBeVisible();
    });

    test("renews an expired post from its owner's email, signed out", async ({ page, browser }) => {
        const nickname = await signUp(page, "M");
        await writeTo(page, "overwatch", "MailTester", ["Still looking?"]);

        const owner = await (await browser.newContext()).newPage();
        await openMail(owner, "mail@example.com");
        const email = emails(owner, `${nickname} sent you a message on TeamTavern`);
        await expect(body(email).getByText("Your post has expired")).toBeVisible();
        await body(email).getByRole("link", { name: "Renew" }).click();

        await expectPage(owner, "/games/overwatch");
        await expect(owner.getByText("MailTester's player post is active again for 30 days. Showing what fits it."))
            .toBeVisible();
        await expect(owner.locator(".feed")).toHaveAttribute("aria-busy", "false");
        await expect(card(owner, "MailTester")).not.toHaveClass(/card-expired/);
        expect(new URL(owner.url()).search).toBe("");
    });

    test("confirms a new address by its link, once", async ({ page }) => {
        const nickname = await signUp(page, "C");
        const address = `${nickname.toLowerCase()}@example.com`;

        await openMail(page, address);
        const email = emails(page, "Confirm your email");
        await expect(email).toHaveCount(1);
        await expect(body(email).getByRole("link", { name: "Choose which emails you get" }))
            .toHaveAttribute("href", "/account#emails");
        await body(email).getByRole("link", { name: "Confirm email" }).click();
        await expectPage(page, "/confirm-email");
        await expect(page.getByRole("heading", { name: "Your email is confirmed" })).toBeVisible();

        await openMail(page, address);
        await body(email).getByRole("link", { name: "Confirm email" }).click();
        await expectPage(page, "/confirm-email");
        await expect(page.getByRole("heading", { name: "This link doesn't work" })).toBeVisible();
    });

    test("resets a forgotten password by its link", async ({ page }) => {
        const nickname = await signUp(page, "R");
        const address = `${nickname.toLowerCase()}@example.com`;
        await signOut(page);

        await page.goto("/forgot-password");
        await page.getByLabel("Email").fill(address);
        await page.getByRole("button", { name: "Send link" }).click();
        await expect(page.getByRole("heading", { name: "Check your email" })).toBeVisible();

        await openMail(page, address);
        await body(emails(page, "Password reset")).getByRole("link", { name: "Reset password" }).click();
        await expectPage(page, "/reset-password");
        await page.getByLabel("New password").fill("a-new-password");
        await page.getByRole("button", { name: "Change password" }).click();
        await expect(page.getByRole("heading", { name: "Your password is changed" })).toBeVisible();

        await submitPasswordSignIn(page, address, "a-new-password");
        await expectPage(page, "/");
        await expect(page.getByRole("button", { name: "Account menu" })).toBeVisible();
    });
});
