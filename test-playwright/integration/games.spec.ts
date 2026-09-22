import { expect, test } from "@playwright/test";

// `Database/Seed/Games/` seeds one file per game, and the catalogue lists them by title.
const gameCount = 10;
const game = { handle: "valorant", title: "Valorant" };
const feedPath = `/games/${game.handle}`;

test.describe("the home page", () => {
    test("shows every game's cover with a link to its feed", async ({ page }) => {
        await page.goto("/");

        const grid = page.locator(".cover-grid");
        await expect(grid.locator(".cover")).toHaveCount(gameCount);
        await expect(grid.getByRole("link").first()).toHaveAccessibleName("Apex Legends");
        const tile = grid.getByRole("link", { name: game.title, exact: true });
        await expect(tile).toHaveAttribute("href", feedPath);

        // Every seeded game has to carry a 600x900 cover under `Static/Images/Games`; a
        // missing or misshapen one shows up here as an image that never gets its size.
        const covers = grid.locator(".cover img");
        await expect(covers).toHaveCount(gameCount);
        for (const cover of await covers.all()) {
            await cover.scrollIntoViewIfNeeded();
            await expect(cover).toHaveJSProperty("naturalWidth", 600);
            await expect(cover).toHaveJSProperty("naturalHeight", 900);
        }

        await tile.click();
        await expect(page).toHaveURL(new RegExp(`${feedPath}$`));
        await expect(page.getByRole("heading", { name: game.title, level: 1 })).toBeVisible();
    });
});
