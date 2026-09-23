import { expect, Page, test } from "@playwright/test";
import { signIn } from "../accounts";
import { expectPage } from "../pages";

// Valorant's seeded posts (`stacks/test-seed/players.sql`): ValorantTester's player post,
// GroupTester's group Night Owls, and ExpiredTester's player post, past its 30 days.
const feedPath = "/games/valorant";
const postPath = /^\/games\/valorant\/posts\/\d+$/;

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

const feedSection = (page: Page) => page.locator(".post-feed");

async function openFromFeed(page: Page, name: string) {
    await page.goto(feedPath);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
    await card(page, name).getByRole("link", { name, exact: true }).click();
    await expectPage(page, postPath);
}

test.describe("a post's page", () => {
    test("is the post as an open card, with the game named and no marks", async ({ page }) => {
        await openFromFeed(page, "Night Owls");

        await expect(page.getByRole("heading", { name: "Night Owls", level: 1 })).toBeVisible();
        await expect(page.locator(".card-type")).toHaveText("Valorant group");
        await expect(page.getByText("Usually online")).toBeVisible();
        await expect(page.getByRole("button", { name: "Details" })).toHaveCount(0);
        await expect(page.locator(".fact-fit, .fact-miss")).toHaveCount(0);
        await expect(page.locator(".card .button-primary")).toHaveText("Message");
        await expect(page).toHaveTitle("Night Owls · Valorant group | TeamTavern");
        await expect(page.locator("#meta-robots")).toHaveAttribute("content", "index, follow");
    });

    test("goes back to the feed as it was left", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 600 });
        await page.goto(feedPath);
        await page.getByRole("button", { name: "Looking for", exact: true }).click();
        await page.getByRole("dialog", { name: "Looking for" }).getByLabel("Ranked").check();
        await page.keyboard.press("Escape");
        await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
        const details = card(page, "Night Owls").getByRole("button", { name: "Details" });
        await details.click();
        await expect(details).toHaveAttribute("aria-expanded", "true");

        const target = card(page, "ValorantTester");
        await target.scrollIntoViewIfNeeded();
        const y = await page.evaluate(() => window.scrollY);
        expect(y).toBeGreaterThan(0);

        await target.getByRole("link", { name: "ValorantTester" }).click();
        await expectPage(page, postPath);
        await page.getByRole("link", { name: "Back to Valorant posts" }).click();

        await expectPage(page, feedPath);
        await expect(details).toHaveAttribute("aria-expanded", "true");
        await expect.poll(() => page.evaluate(() => window.scrollY)).toBe(y);
        await expect(page.getByRole("button", { name: "Looking for: Ranked" })).toBeVisible();

        // Forward to the page again, the feed is still behind it.
        await page.goForward();
        await expectPage(page, postPath);
        await expect(page.getByRole("link", { name: "Back to Valorant posts" })).toBeVisible();
        await page.reload();
        await expect(page.getByRole("link", { name: "Back to Valorant posts" })).toBeVisible();
    });

    test("opened from a link, leads into the whole feed, or into what fits the viewer's description", async ({ context }) => {
        const feed = await context.newPage();
        await openFromFeed(feed, "Night Owls");
        const url = feed.url();
        // A link from elsewhere opens the page in a tab with no feed behind it.
        const page = await context.newPage();
        await page.goto(url);

        await expect(page.getByRole("heading", { name: "Night Owls", level: 1 })).toBeVisible();
        await expect(page.getByRole("link", { name: "Back to Valorant posts" })).toHaveCount(0);
        await expect(feedSection(page).getByRole("heading", { name: "More Valorant posts" })).toBeVisible();
        await expect(feedSection(page)).toContainText("Tell us about you, and the posts that fit come first.");
        await expect(feedSection(page)).toContainText("3 active posts");
        await expect(feedSection(page).getByRole("link", { name: "Browse Valorant posts" })).toHaveAttribute("href", feedPath);

        await page.goto(feedPath);
        await page.getByRole("button", { name: "Looking for", exact: true }).click();
        await page.getByRole("dialog", { name: "Looking for" }).getByLabel("Ranked").check();
        await page.keyboard.press("Escape");
        await expect(page.getByRole("button", { name: "Looking for: Ranked" })).toBeVisible();
        await page.goto(url);

        await expect(feedSection(page)).toContainText("Posts that fit you come first.");
        await expect(feedSection(page).locator(".post-feed-described")).toHaveText("Ranked");
        await feedSection(page).getByRole("link", { name: "See what fits you" }).click();
        await expectPage(page, feedPath);
        await expect(page.getByRole("button", { name: "Looking for: Ranked" })).toBeVisible();
    });

    test("is its owner's view of the post, with what fits it", async ({ page }) => {
        // The owner's feed starts from their own posts, which it leaves out.
        await openFromFeed(page, "Night Owls");
        const url = page.url();
        await signIn(page, "group@example.com");
        await page.goto(url);

        await expect(page.locator(".card-own")).toHaveText("Your post");
        await expect(page.getByRole("button", { name: "Edit" })).toBeVisible();
        await expect(page.getByRole("button", { name: "Renew" })).toBeVisible();
        await expect(page.locator(".card-contact")).toHaveCount(0);
        await expect(page.locator(".own-post-state")).toHaveText(/^Active for \d+ more days$/);
        await expect(page.getByText("No conversations yet")).toBeVisible();
        await expect(page.getByText(/Contacts shown/)).toHaveCount(0);

        await expect(feedSection(page).getByRole("heading", { name: "See what fits Night Owls" })).toBeVisible();
        await expect(feedSection(page)).toContainText("Players who fit it come first.");
        await feedSection(page).getByRole("link", { name: "See what fits" }).click();
        await expectPage(page, feedPath);
        await expect(page.getByText("Showing what fits Night Owls, your group post.")).toBeVisible();
    });

    test("tells a visitor an expired post is old, and keeps it out of search engines", async ({ page }) => {
        await openFromFeed(page, "ExpiredTester");

        await expect(page.getByText("This is an older post. ExpiredTester may no longer be looking, but you can still write.")).toBeVisible();
        await expect(page.locator(".card")).toHaveClass(/card-expired/);
        await expect(page.locator("#meta-robots")).toHaveAttribute("content", "noindex");
    });

    test("is gone for a post that isn't there", async ({ page }) => {
        await page.goto(`${feedPath}/posts/999999`);

        await expect(page.getByRole("heading", { name: "This post is gone" })).toBeVisible();
        await expect(page.getByText("Whoever posted it deleted it. The Valorant feed has everyone else who is looking.")).toBeVisible();
        await expect(page.getByRole("link", { name: "Browse Valorant posts" })).toHaveAttribute("href", feedPath);
        await expect(page.locator('meta[name="renderready-status-code"]')).toHaveAttribute("content", "404");
    });

    test("is not found for a game the site doesn't have", async ({ page }) => {
        await page.goto("/games/nonexistent/posts/1");

        await expect(page.getByRole("heading", { name: "Page could not be found." })).toBeVisible();
        await expect(page.locator('meta[name="renderready-status-code"]')).toHaveAttribute("content", "404");
    });
});

// With scripts off, the page is the HTML the prerenderer returned.
test.describe("a bot on a post's page", () => {
    test.use({
        userAgent: "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)",
        javaScriptEnabled: false,
    });

    test("is answered 404 for a post that isn't there", async ({ page }) => {
        test.slow();

        const response = await page.goto(`${feedPath}/posts/999999`, { timeout: 60_000 });

        expect(response?.status()).toBe(404);
        await expect(page.getByRole("heading", { name: "This post is gone" })).toBeVisible();
    });
});
