import { expect, Page, test } from "@playwright/test";
import { signIn } from "../accounts";
import { expectPage } from "../pages";

// Valorant's seeded posts (`stacks/test-seed/players.sql`): ValorantTester's player post
// (Platinum 1, Duelist, Casual), GroupTester's group Night Owls (Platinum 1 to Diamond 3,
// Ranked) and community Radiant Rising (Casual and Ranked), and ExpiredTester's player post,
// past its 30 days. Fewer than a batch, so the feed never offers Load more.
const feedPath = "/games/valorant";
const olderPosts = /^Older posts/;

// The feed from the top: tier headings, the divider and each card's name, in order.
const feedOrder = (page: Page) => page.locator(".feed").locator(".tier-heading, .divider, .card-name");

// The feed is busy from the moment it asks for posts until the latest request answers.
const expectSettled = (page: Page) => expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

// A chip opens its field's editor; Escape closes it and the feed follows the change.
async function describe(page: Page, chip: string, fill: (editor: ReturnType<Page["getByRole"]>) => Promise<unknown>) {
    await page.getByRole("button", { name: chip, exact: true }).click();
    const editor = page.getByRole("dialog", { name: chip });
    await fill(editor);
    await page.keyboard.press("Escape");
    await expect(editor).toHaveCount(0);
    await expectSettled(page);
}

async function describeRankedPlatinum(page: Page) {
    await describe(page, "Looking for", editor => editor.getByLabel("Ranked").check());
    await describe(page, "Rank", editor => editor.getByRole("combobox").selectOption("platinum-2"));
}

test.describe("the feed", () => {
    test("shows every post by activity while nothing is described, the expired one under the divider", async ({ page }) => {
        await page.goto(feedPath);

        await expect(page.getByRole("heading", { name: "Valorant", level: 1 })).toBeVisible();
        await expect(page.getByText("3 active posts")).toBeVisible();
        await expect(feedOrder(page)).toHaveText(
            ["Radiant Rising", "Night Owls", "ValorantTester", olderPosts, "ExpiredTester"]);
        await expect(page.getByRole("link", { name: "Publish post" })).toHaveCount(0);
        await expect(page.getByRole("button", { name: "Load more" })).toHaveCount(0);
    });

    test("puts the posts that fit the description first and marks what fits", async ({ page }) => {
        await page.goto(feedPath);
        await describeRankedPlatinum(page);

        await expect(feedOrder(page)).toHaveText([
            /^Fits you\s*2$/, "Radiant Rising", "Night Owls",
            /^Missing one thing\s*1$/, "ValorantTester",
            olderPosts, "ExpiredTester",
        ]);
        await expect(card(page, "Night Owls").locator(".fact-fit")).toHaveText(["Fits:Platinum 1 – Diamond 3", "Fits:Ranked"]);
        await expect(card(page, "ValorantTester").locator(".fact-miss")).toHaveText(["Doesn't fit:Casual"]);
        await expect(card(page, "ValorantTester").locator(".fact-fit")).toHaveText(["Fits:Platinum 1", "Fits:In-game leader"]);
        await expect(page.getByRole("link", { name: "Publish post" })).toHaveAttribute("href", "/games/valorant/post/player?from=feed");
        await expect(page.getByRole("button", { name: "Load more" })).toHaveCount(0);

        // The description is kept for the game, so it outlasts the page.
        await page.reload();
        await expect(page.getByRole("button", { name: "Looking for: Ranked" })).toBeVisible();
        await expect(feedOrder(page)).toHaveText([
            /^Fits you\s*2$/, "Radiant Rising", "Night Owls",
            /^Missing one thing\s*1$/, "ValorantTester",
            olderPosts, "ExpiredTester",
        ]);
    });

    test("keeps a field's editor open while its options are picked by their names", async ({ page }) => {
        await page.goto(feedPath);
        await page.getByRole("button", { name: "Looking for", exact: true }).click();
        const editor = page.getByRole("dialog", { name: "Looking for" });
        await editor.getByText("Casual", { exact: true }).click();
        await editor.getByText("Ranked", { exact: true }).click();

        await expect(editor.getByLabel("Casual")).toBeChecked();
        await expect(editor.getByLabel("Ranked")).toBeChecked();
    });

    // ValorantTester can lead; ExpiredTester can't, and Night Owls don't ask for a leader.
    test("fits two players when either can lead, and a group that doesn't ask for a leader either way", async ({ page }) => {
        await page.goto(feedPath);
        await page.getByRole("button", { name: "More", exact: true }).click();
        await page.getByRole("button", { name: "In-game leader", exact: true }).click();
        await expectSettled(page);

        await expect(card(page, "ValorantTester").locator(".fact-fit")).toHaveText(["Fits:In-game leader"]);
        await expect(card(page, "ExpiredTester").locator(".fact-fit")).toHaveText(["Fits:In-game leader: you"]);
        await expect(card(page, "Night Owls").locator(".fact-fit, .fact-miss")).toHaveCount(0);
        await expect(card(page, "Night Owls")).not.toContainText("leader");
    });

    test("narrows to one type of post with Showing", async ({ page }) => {
        await page.goto(feedPath);
        await page.getByRole("group", { name: "Showing" }).getByRole("button", { name: "Groups" }).click();

        await expect(feedOrder(page)).toHaveText(["Night Owls"]);
    });

    test("shows a group only players, with no Showing", async ({ page }) => {
        await page.goto(feedPath);
        await page.getByLabel("We're a group looking for players").check();

        await expect(page.getByText("Fill these in about the players you want.")).toBeVisible();
        await expect(page.getByRole("group", { name: "Showing" })).toHaveCount(0);
        await expect(feedOrder(page)).toHaveText(["ValorantTester", olderPosts, "ExpiredTester"]);
    });

    test("starts from the viewer's own post, which leaves the feed until the description is cleared", async ({ page }) => {
        await signIn(page, "valorant@example.com");
        await page.goto(feedPath);

        await expect(page.getByText("Showing what fits your player post.")).toBeVisible();
        await expect(page.getByRole("button", { name: "Rank: Platinum 1" })).toBeVisible();
        await expect(page.getByRole("link", { name: "Publish post" })).toHaveCount(0);
        await expect(card(page, "ValorantTester")).toHaveCount(0);

        await page.getByRole("button", { name: "Clear all" }).click();
        await expect(card(page, "ValorantTester")).toHaveCount(1);
        await expect(page.getByText("Showing what fits your player post.")).toHaveCount(0);
    });

    test("renews the viewer's expired post, which moves up above the divider", async ({ page }) => {
        // RenewTester's Rainbow Six Siege post is past its 30 days.
        await signIn(page, "renew@example.com");
        await page.goto("/games/rainbow-six-siege");
        await page.getByRole("button", { name: "Clear all" }).click();
        await expectSettled(page);
        await expect(card(page, "RenewTester")).toHaveClass(/card-expired/);

        await card(page, "RenewTester").getByRole("button", { name: "Renew" }).click();

        await expect(page.getByRole("status")).toHaveText("Renewed. Your post stays active for 30 days from today.");
        await expectSettled(page);
        await expect(card(page, "RenewTester")).not.toHaveClass(/card-expired/);
        await expect(card(page, "RenewTester").locator(".card-freshness")).toHaveText("Active just now");
        await expect(page.locator(".feed .divider")).toHaveCount(0);
    });

    test("offers to update the viewer's post once the description differs from it", async ({ page }) => {
        await signIn(page, "group@example.com");
        await page.goto(feedPath);

        await expect(page.getByLabel("We're a group looking for players")).toBeChecked();
        await expect(page.getByText("Showing what fits Night Owls, your group post.")).toBeVisible();
        await expect(page.getByRole("button", { name: "Rank range: Platinum 1 – Diamond 3" })).toBeVisible();

        await page.getByRole("button", { name: "Microphone", exact: true }).click();
        await expect(page.getByText("Update Night Owls with this", { exact: false })).toBeVisible();
        await expect(page.getByRole("link", { name: "Update post" })).toHaveAttribute("href", "/games/valorant/post/group?from=feed");
    });

    test("comes back from a post's page as it was left", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 600 });
        await page.goto(feedPath);
        await describeRankedPlatinum(page);
        const details = card(page, "Night Owls").getByRole("button", { name: "Details" });
        await details.click();
        await expect(details).toHaveAttribute("aria-expanded", "true");

        const target = card(page, "ValorantTester");
        await target.scrollIntoViewIfNeeded();
        const y = await page.evaluate(() => window.scrollY);
        expect(y).toBeGreaterThan(0);

        await target.getByRole("link", { name: "ValorantTester" }).click();
        await expectPage(page, /^\/games\/valorant\/posts\/\d+$/);
        await page.goBack();

        await expectPage(page, feedPath);
        await expect(details).toHaveAttribute("aria-expanded", "true");
        await expect.poll(() => page.evaluate(() => window.scrollY)).toBe(y);
        await expect(page.getByRole("button", { name: "Looking for: Ranked" })).toBeVisible();
    });

    test("is not found for a game the site doesn't have", async ({ page }) => {
        await page.goto("/games/nonexistent");

        await expect(page.getByRole("heading", { name: "Page could not be found." })).toBeVisible();
        await expect(page.locator('meta[name="renderready-status-code"]')).toHaveAttribute("content", "404");
    });
});

// Chromium reports India's zone by its old name, which Postgres doesn't know. The feed sends
// the viewer's zone, so it has to name it as the site's list does.
test.describe("a browser that reports its zone by an old name", () => {
    test.use({ timezoneId: "Asia/Calcutta" });

    test("is shown the feed", async ({ page }) => {
        await page.goto(feedPath);
        expect(await page.evaluate(() => Intl.DateTimeFormat().resolvedOptions().timeZone)).toBe("Asia/Calcutta");
        await expectSettled(page);

        await expect(feedOrder(page)).toHaveText(
            ["Radiant Rising", "Night Owls", "ValorantTester", olderPosts, "ExpiredTester"]);
    });
});

// LeaderlessTester's group Last Call in Counter-Strike 2, Ranked, wants a player who can lead.
test.describe("a group that wants an in-game leader", () => {
    test("misses a player who can't lead and fits one who can", async ({ page }) => {
        await page.goto("/games/counter-strike-2");
        await describe(page, "Looking for", editor => editor.getByLabel("Ranked").check());

        const group = card(page, "Last Call");
        await expect(group.locator(".fact-fit")).toHaveText(["Fits:Ranked"]);
        await expect(group.locator(".fact-miss")).toHaveText(["Doesn't fit:Needs an in-game leader"]);

        await page.getByRole("button", { name: "More", exact: true }).click();
        await page.getByRole("button", { name: "In-game leader", exact: true }).click();
        await expectSettled(page);

        await expect(group.locator(".fact-fit")).toHaveText(["Fits:Ranked", "Fits:Needs an in-game leader"]);
        await expect(group.locator(".fact-miss")).toHaveCount(0);
    });
});
