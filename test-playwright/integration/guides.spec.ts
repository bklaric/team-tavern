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

test("a guide that doesn't exist is not found", async ({ page }) => {
    await page.goto("/guides/no-such-guide");

    await expectPage(page, "/guides/no-such-guide");
    await expect(page.getByRole("heading", { name: "Page could not be found.", level: 1 })).toBeVisible();
});
