import { expect, test } from "@playwright/test";
import { expectPage } from "../pages";

const guidePath = "/guides/join-an-esports-team";
const guideHeading = "How to join an esports team";

test("the guides list each guide with its last update and lead to it", async ({ page }) => {
    await page.goto("/guides");
    await expectPage(page, "/guides");
    const entry = page.getByRole("listitem").filter({ has: page.getByRole("link", { name: guideHeading }) });
    await expect(entry).toContainText(/Updated \d{1,2} [A-Z][a-z]+ \d{4}/);

    await entry.getByRole("link", { name: guideHeading }).click();

    await expectPage(page, guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();
    await expect(page.getByText(/^Last updated \d{1,2} [A-Z][a-z]+ \d{4}$/)).toBeVisible();
    await expect(page).toHaveTitle(/^How to join an esports team/);
});

// The guide's text is HTML from the server, and its links to the site's pages move
// within the site as any other in-app link does, without loading it again.
test("a guide's link to a feed opens the feed without a reload", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();
    await page.evaluate(() => { (self as unknown as { stayed: boolean }).stayed = true; });

    await page.getByRole("link", { name: "Valorant players and groups on TeamTavern" }).click();

    await expectPage(page, "/games/valorant");
    await expect(page.getByRole("heading", { name: "Valorant", level: 1 })).toBeVisible();
    expect(await page.evaluate(() => (self as unknown as { stayed?: boolean }).stayed)).toBe(true);
});

test("the guides to joining and making a team lead to each other", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "How to make an esports team" }).click();

    await expectPage(page, "/guides/make-an-esports-team");
    await expect(page.getByRole("heading", { name: "How to make an esports team", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "chart in the join guide" }).click();

    await expectPage(page, guidePath);
    await expect(page.getByRole("figure", { name: /^Groups asking for each role/ })).toBeInViewport();
});

test("the guide to joining a team leads to the Marvel Rivals guide, and that to its game's feed", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "How to build a Marvel Rivals Championship team" }).click();

    await expectPage(page, "/guides/marvel-rivals-championship-team");
    await expect(page.getByRole("heading", { name: "How to build a Marvel Rivals Championship team", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "Marvel Rivals players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/marvel-rivals");
    await expect(page.getByRole("heading", { name: "Marvel Rivals", level: 1 })).toBeVisible();
});

// The guide's text comes after the page loads, so the page, not the browser,
// finds the part of it the address names.
test("an address naming a part of a guide opens the guide at it", async ({ page }) => {
    await page.goto(`${guidePath}#from-amateur-team-to-pro`);

    await expect(page.getByRole("heading", { name: "From amateur team to pro", level: 2 })).toBeInViewport();
    expect(await page.evaluate(() => scrollY)).toBeGreaterThan(0);
});

test("a guide that doesn't exist is not found", async ({ page }) => {
    await page.goto("/guides/no-such-guide");

    await expectPage(page, "/guides/no-such-guide");
    await expect(page.getByRole("heading", { name: "Page could not be found.", level: 1 })).toBeVisible();
});
