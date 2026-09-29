import { expect, Locator, Page, test } from "@playwright/test";
import { signIn, signUp } from "../accounts";
import { expectPage } from "../pages";

// Players new to the run block and report Dota2Tester (`stacks/test-seed/players.sql`), whom
// no other spec messages or counts the conversations of. A block hides only the two players
// it is between, so what one of these players blocks nobody else sees.

const handle = "dota-2";
const owner = "Dota2Tester";

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

// The rows the ⋯ button opens, named for it.
const more = (scope: Locator) => scope.getByRole("group", { name: "More" });

async function openFeed(page: Page) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

// Opens the owner's panel from the feed and writes to them, so the two have a conversation.
async function write(page: Page) {
    await openFeed(page);
    await card(page, owner).locator(".card-contact").click();
    const panel = page.getByRole("dialog", { name: owner });
    await panel.getByRole("textbox", { name: "Message" }).fill("Selling a boost, cheap.");
    await panel.getByRole("button", { name: "Send" }).click();
    await expect(panel.locator(".message-own")).toHaveCount(1);
    return panel;
}

// The inbox has answered once it shows its list or says there is nothing in it.
async function openInbox(page: Page) {
    await page.goto("/messages");
    await expect(page.locator("#inbox-title, .empty-state")).toBeVisible();
}

test.describe("blocking and reporting", () => {
    test("blocking from the panel hides the post and the conversation both ways, and Undo brings them back",
        async ({ browser }) => {
        const writer = await (await browser.newContext()).newPage();
        const ownerPage = await (await browser.newContext()).newPage();

        const nickname = await signUp(writer, "B");
        const panel = await write(writer);
        const postPath = await card(writer, owner).getByRole("link", { name: owner, exact: true }).getAttribute("href");

        await panel.getByRole("button", { name: "More" }).click();
        await expect(more(panel).getByRole("button")).toHaveText(["Report this post", `Block ${owner}`]);
        await more(panel).getByRole("button", { name: `Block ${owner}` }).click();
        const confirm = panel.getByRole("alertdialog", { name: `Block ${owner}?` });
        await expect(confirm).toHaveAccessibleDescription(
            "You won't see each other's posts, your conversations leave both inboxes, and neither of you hears about the other's new posts. Nothing is deleted: unblocking brings it all back.");
        await expect(confirm.getByRole("button", { name: "Cancel" })).toBeFocused();
        await confirm.getByRole("button", { name: `Block ${owner}` }).click();

        await expect(panel).toHaveCount(0);
        await expect(writer.locator(".toast-text")).toHaveText(`${owner} is blocked.`);
        await expect(card(writer, owner)).toHaveCount(0);

        await writer.locator(".toast").getByRole("button", { name: "Undo" }).click();
        await expect(writer.locator(".toast-text")).toHaveText(`${owner} is unblocked.`);
        await expect(card(writer, owner)).toHaveCount(1);

        await card(writer, owner).locator(".card-contact").click();
        await panel.getByRole("button", { name: "More" }).click();
        await more(panel).getByRole("button", { name: `Block ${owner}` }).click();
        await panel.getByRole("button", { name: `Block ${owner}` }).click();
        await expect(card(writer, owner)).toHaveCount(0);

        await openInbox(writer);
        await expect(writer.getByRole("heading", { name: "No messages yet" })).toBeVisible();

        await signIn(ownerPage, `${handle}@example.com`);
        await openInbox(ownerPage);
        await expect(ownerPage.locator(".inbox-row").filter({ hasText: nickname })).toHaveCount(0);

        await writer.goto(postPath!);
        await expect(writer.getByText(`You blocked ${owner}, so there's no way to contact this post.`)).toBeVisible();
        await expect(writer.locator(".card-contact")).toHaveCount(0);
    });

    test("a report from the panel asks what's wrong, and leaves the post be", async ({ page }) => {
        await signUp(page, "R");
        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });

        await panel.getByRole("button", { name: "More" }).click();
        await more(panel).getByRole("button", { name: "Report this post" }).click();
        await expect(panel.getByRole("heading", { name: `Report ${owner}` })).toBeVisible();
        await expect(panel.getByRole("radio", { name: "Spam or advertising" })).toBeFocused();
        await panel.getByRole("button", { name: "Send report" }).click();
        await expect(panel.getByRole("alert")).toHaveText("Choose what's wrong.");

        await panel.getByRole("radio", { name: "Selling accounts, boosting or cheats" }).check();
        await expect(panel.getByRole("alert")).toHaveCount(0);
        await panel.getByLabel("Anything we should know?").fill("Offers boosts in the post.");
        await panel.getByRole("button", { name: "Send report" }).click();

        await expect(page.locator(".toast-text")).toHaveText("Report sent. Thanks for telling us.");
        await expect(panel.getByRole("textbox", { name: "Message" })).toBeVisible();
        await page.keyboard.press("Escape");
        await expect(panel).toHaveCount(0);
        await expect(card(page, owner)).toHaveCount(1);
    });

    test("the owner reports a writer from the conversation and blocks them with it", async ({ browser }) => {
        const writer = await (await browser.newContext()).newPage();
        const ownerPage = await (await browser.newContext()).newPage();

        const nickname = await signUp(writer, "W");
        await write(writer);

        await signIn(ownerPage, `${handle}@example.com`);
        await openInbox(ownerPage);
        await ownerPage.locator(".inbox-row").filter({ hasText: nickname }).click();
        await expectPage(ownerPage, /^\/messages\/\d+$/);
        const conversation = ownerPage.getByRole("region", { name: nickname });

        await conversation.getByRole("button", { name: "More" }).click();
        await more(conversation).getByRole("button", { name: `Report ${nickname}` }).click();
        await expect(conversation.getByRole("textbox", { name: "Message" })).toHaveCount(0);
        // Escape leaves the report for the conversation.
        await ownerPage.keyboard.press("Escape");
        await expect(conversation.getByRole("textbox", { name: "Message" })).toBeVisible();

        await conversation.getByRole("button", { name: "More" }).click();
        await more(conversation).getByRole("button", { name: `Report ${nickname}` }).click();
        await conversation.getByRole("radio", { name: "Harassment, hate or threats" }).check();
        await conversation.getByLabel(`Also block ${nickname}`).check();
        await conversation.getByRole("button", { name: "Send report" }).click();

        await expect(ownerPage.locator(".toast-text")).toHaveText(`Report sent. ${nickname} is blocked.`);
        await expectPage(ownerPage, "/messages");
        await expect(ownerPage.locator("#inbox-title, .empty-state")).toBeVisible();
        await expect(ownerPage.locator(".inbox-row").filter({ hasText: nickname })).toHaveCount(0);

        await openInbox(writer);
        await expect(writer.getByRole("heading", { name: "No messages yet" })).toBeVisible();
    });
});
