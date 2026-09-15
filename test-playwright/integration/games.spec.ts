import { expect, test } from "@playwright/test";

// `Database/Seed/Games/` seeds one file per game, and `Valorant.sql` carries this description.
const gameCount = 11;
const game = {
    handle: "valorant",
    title: "Valorant",
    description: "Find Valorant teammates for unrated, competitive, spike rush matches and more.",
};

const listingPath = `/games/${game.handle}/players`;

// Caddy answers the retired paths with a redirect before anything else sees them, so the page
// the browser lands on was reached through a 301, not through the site replacing the path itself.
test.describe("a retired path", () => {
    test("a game's bare path lands on its player listing", async ({ page }) => {
        const response = await page.goto(`/games/${game.handle}`);

        const redirect = await response?.request().redirectedFrom()?.response();
        expect(redirect?.status()).toBe(301);
        await expect(page).toHaveURL(new RegExp(`${listingPath}$`));
    });

    test("the game list lands on the home page", async ({ page }) => {
        const response = await page.goto("/games");

        const redirect = await response?.request().redirectedFrom()?.response();
        expect(redirect?.status()).toBe(301);
        await expect(page).toHaveURL(/\/$/);
    });
});

test.describe("the home page", () => {
    test("shows every game's cover with a link to its player listing", async ({ page }) => {
        await page.goto("/");

        const grid = page.locator("#games");
        await expect(grid.locator(".home-game")).toHaveCount(gameCount);
        const tile = grid.getByRole("link", { name: game.title }).first();
        await expect(tile).toHaveAttribute("href", listingPath);

        // Every seeded game has to carry a 600x900 cover under `Static/Images/Games`; a
        // missing or misshapen one shows up here as an image that never gets its size.
        const covers = grid.locator(".home-game img");
        await expect(covers).toHaveCount(gameCount);
        for (const cover of await covers.all()) {
            await cover.scrollIntoViewIfNeeded();
            await expect(cover).toHaveJSProperty("naturalWidth", 600);
            await expect(cover).toHaveJSProperty("naturalHeight", 900);
        }

        await tile.click();
        await expect(page).toHaveURL(new RegExp(`${listingPath}$`));
    });
});

test.describe("a listing page", () => {
    test("describes its game", async ({ page }) => {
        await page.goto(listingPath);

        await expect(page.getByText(game.description)).toBeVisible();
    });

    // Desktop Chrome is wide enough for the header to name the current game in full, and
    // that name is the dropdown's button.
    test("opens the game dropdown from the header", async ({ page }) => {
        await page.goto(listingPath);

        await page.getByRole("button", { name: game.title }).click();

        const popover = page.locator(".top-bar-games-popover");
        await expect(popover.locator(".top-bar-game")).toHaveCount(gameCount);
        await expect(popover.getByRole("link", { name: "Apex Legends" }))
            .toHaveAttribute("href", "/games/apex/players");
    });
});
