import { expect, Page, test } from "@playwright/test";
import { signIn, signOut } from "../accounts";
import { expectPage } from "../pages";

// OwnerTester's posts (`stacks/test-seed/players.sql`): the group Kestrel's Nest in Dota 2,
// active; a player post in Heroes of the Storm, expired 15 days ago; and one in Valheim, in
// its last week. Ten games are seeded, so seven are the other games.
const owner = "owner@example.com";

const game = (page: Page, title: string) => page.getByRole("region", { name: title, exact: true });

async function expectStart(page: Page) {
    await expect(page.getByRole("heading", { name: "What are you posting?", level: 1 })).toBeVisible();
    await expect(page.locator(".type-card")).toHaveCount(3);
    await expect(page.locator(".type-card").nth(0)).toHaveAttribute("href", "/post/player");
    await expect(page.locator(".type-card").nth(1)).toHaveAttribute("href", "/post/group");
    await expect(page.locator(".type-card").nth(2)).toHaveAttribute("href", "/post/community");
    const games = page.getByRole("region", { name: "Or browse a game" });
    await expect(games.locator(".cover")).toHaveCount(10);
}

test.describe("the home page", () => {
    test("asks a visitor what they are posting, and shows every game", async ({ page }) => {
        await page.goto("/");

        await expectStart(page);
        await expect(page).toHaveTitle("TeamTavern: find players, groups and communities");
        await page.locator(".type-card").nth(1).click();
        await expectPage(page, "/post/group");
    });

    test("asks a player without posts the same", async ({ page }) => {
        await signIn(page, "new@example.com");

        await expectStart(page);
    });

    test("is a player's posts by game, each saying how long it stays active", async ({ page }) => {
        await signIn(page, owner);

        await expect(page.getByRole("heading", { name: "Your posts", level: 1 })).toBeVisible();
        await expect(page).toHaveTitle("Your posts | TeamTavern");
        await expect(page.locator(".home-game-name")).toHaveText(["Dota 2", "Heroes of the Storm", "Valheim"]);

        const dota = game(page, "Dota 2");
        await expect(dota.locator(".card-name")).toHaveText("Kestrel's Nest");
        await expect(dota.locator(".card-text")).toHaveCount(0);
        await expect(dota.locator(".own-post-state")).toHaveText("Active for 30 more days");
        await expect(dota.getByRole("button", { name: "Renew" })).toHaveClass(/button-text/);

        const hots = game(page, "Heroes of the Storm");
        await expect(hots.locator(".own-post-state"))
            .toHaveText("Expired 2 weeks ago. It's listed under older posts, and match emails are paused.");
        await expect(hots.getByRole("button", { name: "Renew" })).toHaveClass(/button-outline/);

        const valheim = game(page, "Valheim");
        await expect(valheim.locator(".own-post-state")).toHaveText("Expires in 4 days");
        await expect(valheim.getByRole("button", { name: "Renew" })).toHaveClass(/button-outline/);

        await expect(page.getByRole("region", { name: "Other games" }).locator(".cover")).toHaveCount(7);

        await dota.getByRole("link", { name: "New Dota 2 post" }).click();
        await expectPage(page, "/post");
        await expect(page.getByRole("heading", { name: "What are you posting?" })).toBeVisible();
    });

    test("edits a post, and shows what fits it", async ({ page }) => {
        await signIn(page, owner);

        await game(page, "Dota 2").getByRole("link", { name: "Edit" }).click();
        await expectPage(page, "/games/dota-2/post/group");
        await expect(page.getByRole("heading", { name: "Edit your group post" })).toBeVisible();

        await page.goto("/");
        await game(page, "Dota 2").getByRole("link", { name: "See what fits" }).click();
        await expectPage(page, "/games/dota-2");
        await expect(page.getByText("Showing what fits Kestrel's Nest, your group post.")).toBeVisible();
    });

    // Last, since it renews the expired post the test before reads.
    test("renews a post where it stands", async ({ page }) => {
        await signIn(page, owner);
        const hots = game(page, "Heroes of the Storm");
        const renew = hots.getByRole("button", { name: "Renew" });

        await renew.click();

        await expect(page.getByRole("status")).toHaveText("Renewed. Your post stays active for 30 days from today.");
        await expect(hots.locator(".own-post-state")).toHaveText("Active for 30 more days");
        await expect(renew).toHaveClass(/button-text/);
        await expect(renew).toBeFocused();
        await expect(page.locator(".home-game-name")).toHaveText(["Dota 2", "Heroes of the Storm", "Valheim"]);

        await page.reload();
        await expect(hots.locator(".own-post-state")).toHaveText("Active for 30 more days");

        // The owner's feed leaves their own posts out, so the feed is read signed out.
        await signOut(page);
        await page.goto("/games/heroes-of-the-storm");
        await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
        await expect(page.locator(".feed .divider")).toHaveCount(0);
        const card = page.locator(".card").filter({ has: page.getByRole("link", { name: "OwnerTester", exact: true }) });
        await expect(card.locator(".card-freshness")).toHaveText("Active just now");
    });
});
