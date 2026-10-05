import { expect, test, type Locator } from "@playwright/test";
import { expectPage } from "../pages";

// `Database/Seed/Games/` seeds one file per game, and the catalogue lists them by title.
const gameCount = 13;
const game = { handle: "valorant", title: "Valorant" };
const feedPath = `/games/${game.handle}`;

// The file a cover has loaded, once it has.
const loadedFile = (cover: Locator) =>
    cover.evaluate((image: HTMLImageElement) =>
        image.complete && image.naturalWidth > 0 ? new URL(image.currentSrc).pathname : null);

test.describe("the home page", () => {
    test("shows every game's cover with a link to its feed", async ({ page }) => {
        await page.goto("/");

        const grid = page.locator(".cover-grid");
        await expect(grid.locator(".cover")).toHaveCount(gameCount);
        await expect(grid.getByRole("link").first()).toHaveAccessibleName("Apex Legends");
        const tile = grid.getByRole("link", { name: game.title, exact: true });
        await expect(tile).toHaveAttribute("href", feedPath);

        // Every seeded game has to carry a 600x900 cover under `Static/Images/Games`, which
        // `build-covers.mjs` checks, and the 400x600 copy it makes of it is what a 160px
        // tile loads. A missing one shows up here as an image that never loads.
        const tiles = grid.getByRole("link");
        await expect(tiles).toHaveCount(gameCount);
        for (const link of await tiles.all()) {
            const handle = (await link.getAttribute("href"))!.replace("/games/", "");
            const cover = link.locator("img");
            await cover.scrollIntoViewIfNeeded();
            await expect.poll(() => loadedFile(cover)).toBe(`/images/games/400/${handle}.webp`);
        }

        await tile.click();
        await expectPage(page, feedPath);
        await expect(page.getByRole("heading", { name: `${game.title} LFG`, level: 1 })).toBeVisible();
    });
});

// A phone's tile is a third of its width, so a narrow one at 3x still makes do with the
// copy, and only a wide one needs the original.
for (const [width, file] of [[375, `/images/games/400/${game.handle}.webp`], [600, `/images/games/${game.handle}.webp`]] as const)
    test.describe(`a ${width} px window at 3x`, () => {
        test.use({ viewport: { width, height: 800 }, deviceScaleFactor: 3 });

        test(`loads ${file} for a home page tile`, async ({ page }) => {
            await page.goto("/");

            const cover = page.locator(".cover-grid").getByRole("link", { name: game.title, exact: true }).locator("img");
            await cover.scrollIntoViewIfNeeded();
            await expect.poll(() => loadedFile(cover)).toBe(file);
        });
    });
