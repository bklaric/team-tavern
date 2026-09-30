import { expect, Page, test } from "@playwright/test";
import { expectPage } from "../pages";

// The Venatus script fills nothing on a local origin, so this one stands in for it. Like
// the real one it drains the queue the page has built up and then runs whatever is pushed
// at once, and it draws each unit as a box of the unit's size, in the element given or on
// the window's floor. A unit in the page sits inline-block inside a span, as Google's
// frame does. The interstitial `index.html` asks for is not drawn.
const fakeVenatus = `(() => {
    const sizes = {
        skyscraper: "display: inline-block; width: 160px; height: 600px",
        desktop_takeover: "display: inline-block; width: 970px; height: 250px",
        horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 728px; height: 90px",
        mobile_horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 320px; height: 50px",
    };
    const draw = (name, parent) => {
        if (!sizes[name]) {
            return { remove() {} };
        }
        const frame = document.createElement("div");
        frame.dataset.fakeAd = name;
        frame.style.cssText = sizes[name];
        const node = document.createElement("span");
        node.appendChild(frame);
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

const guidePath = "/guides/join-an-esports-team";

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

    test("keep the takeover's room before it fills, so the page stays put when it does", async ({ page }) => {
        await page.unroute("https://hb.vntsm.com/**");
        await page.route("https://hb.vntsm.com/**", route => route.fulfill({ contentType: "text/javascript", body: "" }));
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        const before = await box(page, ".feed-header");

        await page.addScriptTag({ content: fakeVenatus });
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        const after = await box(page, ".feed-header");
        expect(after.y).toBe(before.y);
    });

    // An ad blocker refuses the script and hides every element whose class its filter
    // lists name, which are classes that say ad. Only the units may go with them.
    test("take nothing of the page with them when an ad blocker hides them", async ({ page }) => {
        await page.unroute("https://hb.vntsm.com/**");
        await page.route("https://hb.vntsm.com/**", route => route.abort());
        const hideAdClasses = () => page.addStyleTag({ content: `
            .ad, .ads, [class^="ad-"], [class*=" ad-"], [class^="ads-"], [class*=" ads-"] { display: none !important; }
        ` });
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await hideAdClasses();

        await expect(page.getByRole("heading", { name: "Valorant LFG", level: 1 })).toBeVisible();
        await expect(page.locator(".card").first()).toBeVisible();
        const column = await box(page, ".feed-page");
        const middle = await page.evaluate(() => document.documentElement.clientWidth / 2);
        expect(Math.abs(column.x + column.width / 2 - middle)).toBeLessThanOrEqual(1);

        await page.goto(guidePath);
        await hideAdClasses();
        await expect(page.getByRole("heading", { name: "How to join an esports team", level: 1 })).toBeVisible();
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

    test("are the same on the guides and on a guide, around its column", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await page.goto("/guides");
        await expect(page.getByRole("heading", { name: "Guides", level: 1 })).toBeVisible();
        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();

        await page.getByRole("link", { name: "How to join an esports team" }).click();
        await expectPage(page, guidePath);
        const heading = page.getByRole("heading", { name: "How to join an esports team", level: 1 });
        await expect(heading).toBeVisible();

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();

        const column = await box(page, ".document");
        const left = await box(page, ".ad-rail-left [data-fake-ad]");
        const right = await box(page, ".ad-rail-right [data-fake-ad]");
        const middle = await page.evaluate(() => document.documentElement.clientWidth / 2);
        expect(Math.abs(column.x + column.width / 2 - middle)).toBeLessThanOrEqual(1);
        expect(left.x + left.width).toBeLessThanOrEqual(column.x);
        expect(right.x).toBeGreaterThanOrEqual(column.x + column.width);

        const takeover = await box(page, "[data-fake-ad=desktop_takeover]");
        expect(takeover.y + takeover.height).toBeLessThanOrEqual((await heading.boundingBox())!.y);
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

    test("are the same one sticky on a guide", async ({ page }) => {
        await page.goto(guidePath);
        await expect(page.getByRole("heading", { name: "How to join an esports team", level: 1 })).toBeVisible();

        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(page.locator("[data-fake-ad]")).toHaveCount(1);
    });
});
