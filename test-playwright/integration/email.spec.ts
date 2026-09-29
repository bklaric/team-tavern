import { expect, Locator, Page, test } from "@playwright/test";
import { signIn, signOut, signUp, submitPasswordSignIn } from "../accounts";
import { body, emails, openMail } from "../mail";
import { expectPage } from "../pages";

// MailTester and QuietTester (`stacks/test-seed/players.sql`) are written to only
// here; the players writing to them are new to each test. FitsTester and ExpiringTester
// are here for the worker's period email, which the test stack sends every two seconds.

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

// The mail page shows what had come when it was opened, and a period's email comes when
// the period ends, so the page is opened again until it is there.
async function periodEmail(page: Page, address: string, subject: string): Promise<Locator> {
    const email = emails(page, subject);
    await expect(async () => {
        await openMail(page, address);
        await expect(email).toHaveCount(1, { timeout: 1_000 });
    }).toPass({ timeout: 15_000 });
    return email;
}

const bell = (page: Page) => page.getByRole("button", { name: /^Notifications/ });

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

    test("tells an owner in the period's email of a post that fits theirs, and opens it", async ({ page, browser }) => {
        await signIn(page, "fits@example.com");
        await page.getByRole("region", { name: "Apex Legends", exact: true }).getByRole("button", { name: "Renew" }).click();
        await expect(page.getByRole("status")).toHaveText("Renewed. Your post stays active for 30 days from today.");

        const owner = await (await browser.newContext()).newPage();
        const email = await periodEmail(owner, "apex-legends@example.com", "A new post fits yours");
        await expect(body(email).getByRole("heading", { name: "Your Apex Legends player post" })).toBeVisible();
        await expect(body(email).getByText("A new post fits it:")).toBeVisible();
        await expect(body(email).getByRole("link", { name: "Renew" })).toHaveCount(0);

        await body(email).getByRole("link", { name: "FitsTester · player post" }).click();
        await expectPage(owner, /^\/games\/apex-legends\/posts\/\d+$/);
        await expect(owner.getByRole("heading", { level: 1, name: "FitsTester" })).toBeVisible();
    });

    test("reminds an owner of their posts in their last week in one email, and renews from it", async ({ page }) => {
        const email = await periodEmail(page, "expiring@example.com", "2 of your posts expire soon");
        await expect(body(email).getByRole("heading")).toHaveText([
            "Your Team Fortress 2 player post",
            "Your Team Fortress 2 community Night Shift",
        ]);
        await expect(body(email).getByText(/^It expires in \d+ days?\.$/)).toHaveCount(2);
        // A community is joined through a link nothing checks, so its owner is asked.
        await expect(body(email).getByText(/^Does its Discord invite still work\?/)).toHaveCount(1);
        await expect(body(email).getByText("Renew it and it stays active for 90 days from today:")).toBeVisible();
        const renew = body(email).getByRole("link", { name: "Renew" });
        await expect(renew).toHaveCount(2);

        await renew.nth(1).click();
        await expectPage(page, "/games/team-fortress-2");
        await expect(page.getByText("Night Shift is active again for 90 days. Showing what fits it.")).toBeVisible();
    });

    test("sends nothing about a post in its last week to an owner who switched renewal emails off", async ({ page }) => {
        // The notice on the bell says the worker has had its period with the post, and
        // the period's email would have gone with it.
        await signIn(page, "quiet@example.com");
        await expect(async () => {
            await page.reload();
            await expect(bell(page)).toHaveAccessibleName(/^Notifications, \d+ new$/, { timeout: 1_000 });
        }).toPass({ timeout: 15_000 });
        await bell(page).click();
        const group = page.getByRole("dialog", { name: "Notifications" }).locator(".notification-group")
            .filter({ has: page.getByRole("heading", { name: "QuietTester · Team Fortress 2 player" }) });
        await expect(group.locator(".notification-title")).toHaveText(/^Expires in \d+ days?$/);

        await openMail(page, "quiet@example.com");
        await expect(page.getByText("No emails.")).toBeVisible();
    });
});
