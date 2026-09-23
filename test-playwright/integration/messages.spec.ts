import { expect, Page, test } from "@playwright/test";
import { signIn, signUp } from "../accounts";
import { expectPage } from "../pages";

// A player new to the run writes to HeroesOfTheStormTester (`stacks/test-seed/players.sql`),
// whose post no other spec messages, so its conversations are only this spec's, one per
// writer.

const handle = "heroes-of-the-storm";
const owner = "HeroesOfTheStormTester";

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openFeed(page: Page) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

test.describe("messaging", () => {
    test("starts a conversation from the contact panel, which the card then says", async ({ page }) => {
        await signUp(page, "W");
        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });
        await expect(panel.getByRole("heading", { name: `Message ${owner}` })).toBeVisible();

        const box = panel.getByRole("textbox", { name: "Message" });
        await box.fill("Hi, want to play tonight?");
        await box.press("Shift+Enter");
        await box.pressSequentially("I'm on from nine.");
        await box.press("Enter");

        await expect(panel.locator(".message-own")).toHaveText("You: Hi, want to play tonight?\nI'm on from nine.");
        await expect(box).toHaveValue("");
        await expect(panel.getByRole("heading", { name: `Your conversation with ${owner}` })).toBeVisible();
        await page.keyboard.press("Escape");
        await expect(card(page, owner).locator(".card-messaged")).toHaveText("You messaged just now");
        await expect(card(page, owner).locator(".card-contact")).toHaveText("Open conversation");

        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        await expect(panel.locator(".message-own")).toHaveText(["You: Hi, want to play tonight?\nI'm on from nine."]);
        await expect(panel.locator(".thread-new")).toHaveCount(0);
    });

    // Two players, each in a browser of their own. Earlier tests leave the owner other
    // conversations, unread, so the owner's counts are read as they change.
    test("reaches the owner, who replies from the inbox", async ({ browser }) => {
        const writer = await (await browser.newContext()).newPage();
        const owner_ = await (await browser.newContext()).newPage();

        const nickname = await signUp(writer, "W");
        await openFeed(writer);
        await card(writer, owner).locator(".card-contact").click();
        const panel = writer.getByRole("dialog", { name: owner });
        await panel.getByRole("textbox", { name: "Message" }).fill("Hi, want to play tonight?");
        await panel.getByRole("button", { name: "Send" }).click();
        await expect(panel.locator(".message-own")).toHaveCount(1);

        await signIn(owner_, `${handle}@example.com`);
        const inboxLink = owner_.getByRole("link", { name: /^Messages/ });
        await expect(inboxLink).toHaveAccessibleName(/^Messages, \d+ unread$/);
        await inboxLink.click();
        await expectPage(owner_, "/messages");
        const row = owner_.locator(".inbox-row").filter({ hasText: nickname });
        await expect(row).toHaveClass(/inbox-row-unread/);
        await expect(row.locator(".inbox-row-snippet")).toHaveText("Hi, want to play tonight?");

        // The home page's count opens the latest unread conversation, which is this one.
        await owner_.getByRole("link", { name: "TeamTavern" }).click();
        await expectPage(owner_, "/");
        await owner_.locator("a.own-post-stat").click();
        await expectPage(owner_, /^\/messages\/\d+$/);
        const conversation = owner_.getByRole("region", { name: nickname });
        await expect(conversation.locator(".conversation-context"))
            .toHaveText(`About your post ${owner} · Heroes of the Storm player`);
        await expect(conversation.locator(".thread-new + .message")).toHaveText(`${nickname}: Hi, want to play tonight?`);
        await expect(row).not.toHaveClass(/inbox-row-unread/);

        const box = conversation.getByRole("textbox", { name: "Message" });
        await box.fill("Sure, see you at nine.");
        await box.press("Enter");
        await expect(conversation.locator(".message-own")).toHaveText(["You: Sure, see you at nine."]);
        await expect(conversation.locator(".thread-new")).toHaveCount(0);
        await expect(row.locator(".inbox-row-snippet")).toHaveText("You: Sure, see you at nine.");

        await openFeed(writer);
        await expect(writer.getByRole("link", { name: "Messages, 1 unread" })).toBeVisible();
        await card(writer, owner).locator(".card-contact").click();
        await expect(panel.locator(".thread-new + .message")).toHaveText(`${owner}: Sure, see you at nine.`);
        await expect(writer.getByRole("link", { name: "Messages", exact: true })).toBeVisible();
        await writer.keyboard.press("Escape");

        await writer.getByRole("link", { name: "Messages", exact: true }).click();
        await expectPage(writer, "/messages");
        await expect(writer.getByRole("heading", { name: "Posts you messaged" })).toBeVisible();
        const messaged = writer.locator(".inbox-row").filter({ hasText: owner });
        await expect(messaged.locator(".inbox-row-snippet")).toHaveText(`${owner}: Sure, see you at nine.`);
        await messaged.click();
        await expectPage(writer, /^\/messages\/\d+$/);
        await expect(writer.getByRole("region", { name: owner }).locator(".conversation-context"))
            .toHaveText(/^Heroes of the Storm player · Active /);
    });

    test("asks a visitor to sign in, and tells a player without conversations how to start one", async ({ page }) => {
        await page.goto("/messages");
        await expect(page.getByRole("heading", { name: "Sign in to see your messages" })).toBeVisible();
        await expect(page.getByRole("link", { name: "Sign in" }).last()).toHaveAttribute("href", "/signin?back=%2Fmessages");

        await signUp(page, "W");
        await page.goto("/messages");
        await expect(page.getByRole("heading", { name: "No messages yet" })).toBeVisible();
    });

    test("sends nothing that is only blank", async ({ page }) => {
        await signUp(page, "W");
        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });

        await panel.getByRole("textbox", { name: "Message" }).fill("   ");
        await panel.getByRole("button", { name: "Send" }).click();

        await expect(panel.locator(".message")).toHaveCount(0);
        await expect(panel.getByText("Your message starts a conversation about HeroesOfTheStormTester's post."))
            .toBeVisible();
    });
});
