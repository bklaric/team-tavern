import { expect, Page, test } from "@playwright/test";
import { expectPage } from "../pages";

// The Venatus script fills nothing on a local origin, so this one stands in for it. Like
// the real one it drains the queue the page has built up and then runs whatever is pushed
// at once, and it draws each unit as a box of the unit's size, in the element given or on
// the window's floor. The interstitial `index.html` asks for is not drawn.
const fakeVenatus = `(() => {
    const sizes = {
        skyscraper: "width: 160px; height: 600px",
        desktop_takeover: "width: 970px; height: 250px",
        horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 728px; height: 90px",
        mobile_horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 320px; height: 50px",
    };
    const draw = (name, parent) => {
        if (!sizes[name]) {
            return { remove() {} };
        }
        const node = document.createElement("div");
        node.dataset.fakeAd = name;
        node.style.cssText = sizes[name];
        parent.appendChild(node);
        return { remove: () => node.remove() };
    };
    const scope = {
        Config: {
            get: name => ({
                display: element => draw(name, element),
                displayBody: () => draw(name, document.body),
            }),
            autoPlacements: () => ({ remove() {} }),
        },
        Instances: { pageManager: { on() {}, newPageSession() {} } },
    };
    const pending = self.__VM || [];
    self.__VM = { push: callback => callback({}, scope) };
    pending.forEach(callback => self.__VM.push(callback));
})();`;

const feedPath = "/games/valorant";

const unit = (page: Page, name: string) => page.locator(`[data-fake-ad="${name}"]`);

async function openFeed(page: Page) {
    await page.goto(feedPath);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

async function box(page: Page, selector: string) {
    const found = await page.locator(selector).boundingBox();
    expect(found).not.toBeNull();
    return found!;
}

test.beforeEach(async ({ page }) => {
    await page.route("https://hb.vntsm.com/**", route =>
        route.fulfill({ contentType: "text/javascript", body: fakeVenatus }));
});

test.describe("the ads", () => {
    test("flank the feed's column on a desktop, with a takeover above it and a sticky below", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();
        await expect(unit(page, "mobile_horizontal_sticky")).toHaveCount(0);

        // The column stays where it was, in the middle, and the rails keep out of it.
        const column = await box(page, ".feed-page");
        const left = await box(page, ".ad-rail-left [data-fake-ad]");
        const right = await box(page, ".ad-rail-right [data-fake-ad]");
        const middle = await page.evaluate(() => document.documentElement.clientWidth / 2);
        expect(Math.abs(column.x + column.width / 2 - middle)).toBeLessThanOrEqual(1);
        expect(left.x + left.width).toBeLessThanOrEqual(column.x);
        expect(right.x).toBeGreaterThanOrEqual(column.x + column.width);

        const takeover = await box(page, "[data-fake-ad=desktop_takeover]");
        const header = await box(page, ".feed-header");
        expect(takeover.y + takeover.height).toBeLessThanOrEqual(header.y);
    });

    test("drop the rails where a skyscraper no longer fits beside the column", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await expect(unit(page, "skyscraper")).toHaveCount(2);

        await page.setViewportSize({ width: 1024, height: 900 });
        await expect(unit(page, "skyscraper")).toHaveCount(0);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();

        await page.setViewportSize({ width: 1280, height: 900 });
        await expect(unit(page, "skyscraper")).toHaveCount(2);
    });

    test("are the same on a post's page", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await page.locator(".card").getByRole("link", { name: "Night Owls", exact: true }).click();
        await expectPage(page, /^\/games\/valorant\/posts\/\d+$/);
        await expect(page.getByRole("heading", { name: "Night Owls", level: 1 })).toBeVisible();

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toHaveCount(1);
        await expect(unit(page, "horizontal_sticky")).toHaveCount(1);
    });

    test("leave with the page, and the home page has none", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await expect(unit(page, "skyscraper")).toHaveCount(2);

        await page.getByRole("link", { name: "TeamTavern", exact: true }).click();
        await expectPage(page, "/");

        await expect(page.locator("[data-fake-ad]")).toHaveCount(0);
    });
});

test.describe("the ads on a phone", () => {
    test.use({ viewport: { width: 375, height: 740 } });

    test("are one sticky, which steps aside while an overlay is open", async ({ page }) => {
        await openFeed(page);

        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(page.locator("[data-fake-ad]")).toHaveCount(1);

        await page.locator(".description-summary").click();
        await expect(page.getByRole("dialog", { name: "Tell us about you" })).toBeVisible();
        await expect(unit(page, "mobile_horizontal_sticky")).toHaveCount(0);

        await page.keyboard.press("Escape");
        await expect(page.getByRole("dialog", { name: "Tell us about you" })).toHaveCount(0);
        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
    });
});
