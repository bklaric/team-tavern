import { expect, Page, test } from "@playwright/test";
import { signUp } from "../accounts";
import { expectPage } from "../pages";

// The Venatus script fills nothing on a local origin, so this one stands in for it. Like
// the real one it drains the queue the page has built up and then runs whatever is pushed
// at once, and it draws each unit as a box of one of the unit's sizes, in the element given
// or over the window. A unit in the page sits inline-block inside a span, as Google's frame
// does. The interstitial `index.html` asks for is not drawn.
const fakeVenatus = `(() => {
    const inPage = (width, height) => "display: inline-block; width: " + width + "px; height: " + height + "px";
    const sizes = {
        desktop_takeover: inPage(970, 250),
        skyscraper: inPage(160, 600),
        double_mpu: inPage(300, 600),
        billboard: inPage(728, 250),
        leaderboard: inPage(728, 90),
        mobile_takeover: inPage(300, 250),
        mobile_mpu: inPage(336, 280),
        mobile_banner: inPage(320, 50),
        horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 728px; height: 90px",
        mobile_horizontal_sticky: "position: fixed; bottom: 0; left: 0; width: 320px; height: 50px",
        video_slider: "position: fixed; bottom: 70px; right: 5px; width: 481px; aspect-ratio: 16 / 9",
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

// Valorant's feed holds four posts, three active and one expired (`stacks/test-seed/players.sql`),
// so it has a unit after its third card and too few cards for one after a sixth.
const feedPath = "/games/valorant";

// Team Fortress 2's holds six active posts, so it has a unit after its sixth card too.
const longFeedPath = "/games/team-fortress-2";

// A unit after the given card of six, and so before the next card if there is one.
async function expectAfterCard(page: Page, name: string, card: number) {
    const cards = page.locator(".card");
    await expect(cards).toHaveCount(6);
    const before = await boxOf(cards.nth(card - 1));
    const found = await box(page, `[data-fake-ad=${name}]`);
    expect(found.y).toBeGreaterThanOrEqual(before.y + before.height);
    if (card < 6) {
        expect(found.y + found.height).toBeLessThanOrEqual((await boxOf(cards.nth(card))).y);
    }
}

const guidePath = "/guides/join-an-esports-team";

const desktopUnits = ["desktop_takeover", "skyscraper", "double_mpu", "billboard", "leaderboard", "horizontal_sticky", "video_slider"];

const mobileUnits = ["mobile_takeover", "mobile_mpu", "mobile_banner", "mobile_horizontal_sticky"];

const unit = (page: Page, name: string) => page.locator(`[data-fake-ad="${name}"]`);

const units = (page: Page, names: string[]) => page.locator(names.map(name => `[data-fake-ad="${name}"]`).join(", "));

async function openFeed(page: Page, path = feedPath) {
    await page.goto(path);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

async function box(page: Page, selector: string) {
    const found = await page.locator(selector).boundingBox();
    expect(found).not.toBeNull();
    return found!;
}

async function boxOf(locator: ReturnType<Page["locator"]>) {
    const found = await locator.boundingBox();
    expect(found).not.toBeNull();
    return found!;
}

// The column sits in the middle of the window and both rails' units keep out of it.
async function expectRailsBeside(page: Page, columnSelector: string) {
    const column = await box(page, columnSelector);
    const left = await box(page, ".ad-rail-left [data-fake-ad]");
    const right = await box(page, ".ad-rail-right [data-fake-ad]");
    const middle = await page.evaluate(() => document.documentElement.clientWidth / 2);
    expect(Math.abs(column.x + column.width / 2 - middle)).toBeLessThanOrEqual(1);
    expect(left.x + left.width).toBeLessThanOrEqual(column.x);
    expect(right.x).toBeGreaterThanOrEqual(column.x + column.width);
    expect(right.x + right.width).toBeLessThanOrEqual(await page.evaluate(() => document.documentElement.clientWidth));
}

test.beforeEach(async ({ page }) => {
    await page.route("https://hb.vntsm.com/**", route =>
        route.fulfill({ contentType: "text/javascript", body: fakeVenatus }));
});

test.describe("the ads", () => {
    // Ad scripts report the address a document was opened with, so a document opened with a
    // secret in its address, a reset, confirmation or renewal nonce or what a provider sent
    // back to the sign-in page, loads none, and the page the player goes on to is loaded
    // afresh, with them. Each page still reads its secret, which the specs of those flows
    // show.
    test("load nowhere a link's secret came in the address", async ({ page }) => {
        await page.route("https://hb.vntsm.com/**", route => route.fulfill({
            contentType: "text/javascript",
            body: `self.__adsSaw = location.href + " " + performance.getEntriesByType("navigation")[0].name;\n${fakeVenatus}`,
        }));
        let requested = 0;
        page.on("request", request => { if (request.url().startsWith("https://hb.vntsm.com/")) requested++; });
        const seen = () => page.evaluate(() => (window as unknown as { __adsSaw?: string }).__adsSaw);
        for (const [path, secret] of [
            ["/reset-password?nonce=resetsecret", "resetsecret"],
            ["/confirm-email?nonce=confirmsecret", "confirmsecret"],
            ["/renew?nonce=renewsecret", "renewsecret"],
            ["/signin?code=codesecret&state=statesecret", "secret"],
            ["/signin?error=access_denied&state=statesecret", "statesecret"],
            ["/signin?steam=trip&openid.mode=id_res&openid.sig=sigsecret", "sigsecret"],
        ]) {
            const before = requested;
            await page.goto(path);
            await expectPage(page, path.split("?")[0]);
            expect(requested, path).toBe(before);

            await Promise.all([
                page.waitForEvent("load"),
                page.getByRole("link", { name: "TeamTavern" }).first().click(),
            ]);
            await expectPage(page, "/");
            await expect.poll(seen).toBeTruthy();
            expect(await seen(), path).not.toContain(secret);
        }
    });

    test("flank the feed's column on a desktop, with a takeover above it, a billboard after its third card and a sticky below", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "double_mpu")).toHaveCount(0);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();
        await expect(unit(page, "billboard")).toBeVisible();
        await expect(unit(page, "leaderboard")).toHaveCount(0);
        await expect(units(page, mobileUnits)).toHaveCount(0);
        await expectRailsBeside(page, ".feed-page");

        const takeover = await box(page, "[data-fake-ad=desktop_takeover]");
        const header = await box(page, ".feed-header");
        expect(takeover.y + takeover.height).toBeLessThanOrEqual(header.y);

        // The billboard is as wide as the cards and sits between the third and the fourth.
        const cards = page.locator(".card");
        await expect(cards).toHaveCount(4);
        const third = await boxOf(cards.nth(2));
        const fourth = await boxOf(cards.nth(3));
        const billboard = await box(page, "[data-fake-ad=billboard]");
        expect(billboard.x).toBeCloseTo(third.x, 0);
        expect(billboard.width).toBeCloseTo(third.width, 0);
        expect(billboard.y).toBeGreaterThanOrEqual(third.y + third.height);
        expect(billboard.y + billboard.height).toBeLessThanOrEqual(fourth.y);
    });

    test("put a billboard after a feed's third card and a leaderboard after its sixth", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page, longFeedPath);

        await expectAfterCard(page, "billboard", 3);
        await expectAfterCard(page, "leaderboard", 6);
        const card = await boxOf(page.locator(".card").first());
        const leaderboard = await box(page, "[data-fake-ad=leaderboard]");
        expect(leaderboard.x).toBeCloseTo(card.x, 0);
        expect(leaderboard.width).toBeCloseTo(card.width, 0);
    });

    test("take double MPUs on either side where they fit", async ({ page }) => {
        await page.setViewportSize({ width: 1600, height: 900 });
        await openFeed(page);

        await expect(unit(page, "double_mpu")).toHaveCount(2);
        await expect(unit(page, "skyscraper")).toHaveCount(0);
        await expectRailsBeside(page, ".feed-page");
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
        await expect(page.locator(".card")).toHaveCount(4);
        const column = await box(page, ".feed-page");
        const middle = await page.evaluate(() => document.documentElement.clientWidth / 2);
        expect(Math.abs(column.x + column.width / 2 - middle)).toBeLessThanOrEqual(1);

        await page.goto(guidePath);
        await hideAdClasses();
        await expect(page.getByRole("heading", { name: "How to join an esports team", level: 1 })).toBeVisible();
        await expect(page.locator(".guide h2").first()).toBeVisible();
    });

    // Venatus counts a window wider than 1024 px a desktop and any other a phone, and a
    // placement fills on one of the two.
    test("follow Venatus's breakpoints as the window changes", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await expect(unit(page, "skyscraper")).toHaveCount(2);

        await page.setViewportSize({ width: 1600, height: 900 });
        await expect(unit(page, "double_mpu")).toHaveCount(2);
        await expect(unit(page, "skyscraper")).toHaveCount(0);

        await page.setViewportSize({ width: 1100, height: 900 });
        await expect(units(page, ["skyscraper", "double_mpu"])).toHaveCount(0);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "billboard")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();

        await page.setViewportSize({ width: 1024, height: 900 });
        await expect(units(page, desktopUnits)).toHaveCount(0);
        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(unit(page, "mobile_banner")).toBeVisible();
        await expect(unit(page, "mobile_takeover")).toBeVisible();

        await page.setViewportSize({ width: 1280, height: 900 });
        await expect(units(page, mobileUnits)).toHaveCount(0);
        await expect(unit(page, "skyscraper")).toHaveCount(2);
    });

    // The video floats over the window's bottom right corner, which clears the right rail
    // below the takeover only in a tall window.
    test("add the feed's video in a window tall enough", async ({ page }) => {
        await page.setViewportSize({ width: 1600, height: 1300 });
        await openFeed(page);
        await expect(unit(page, "video_slider")).toBeVisible();
        const video = await box(page, "[data-fake-ad=video_slider]");
        const rail = await box(page, ".ad-rail-right [data-fake-ad]");
        expect(video.y).toBeGreaterThanOrEqual(rail.y + rail.height);

        await page.setViewportSize({ width: 1600, height: 1200 });
        await expect(unit(page, "video_slider")).toHaveCount(0);
    });

    test("are the same on a post's page, without the feed's units", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await page.locator(".card").getByRole("link", { name: "Night Owls", exact: true }).click();
        await expectPage(page, /^\/games\/valorant\/posts\/\d+$/);
        await expect(page.getByRole("heading", { name: "Night Owls", level: 1 })).toBeVisible();

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toHaveCount(1);
        await expect(unit(page, "horizontal_sticky")).toHaveCount(1);
        await expect(units(page, ["billboard", "leaderboard", "video_slider"])).toHaveCount(0);
    });

    test("are the same on the guides, and a guide has a billboard after its summary and a leaderboard two sections on", async ({ page }) => {
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
        await expectRailsBeside(page, ".document");

        const takeover = await box(page, "[data-fake-ad=desktop_takeover]");
        expect(takeover.y + takeover.height).toBeLessThanOrEqual((await boxOf(heading)).y);

        const summary = await box(page, ".guide blockquote");
        const sections = page.locator(".guide h2");
        const billboard = await box(page, "[data-fake-ad=billboard]");
        expect(billboard.y).toBeGreaterThanOrEqual(summary.y + summary.height);
        expect(billboard.y + billboard.height).toBeLessThanOrEqual((await boxOf(sections.nth(0))).y);
        expect(billboard.width).toBeCloseTo(summary.width, 0);

        const leaderboard = await box(page, "[data-fake-ad=leaderboard]");
        expect(leaderboard.y).toBeGreaterThan((await boxOf(sections.nth(1))).y);
        expect(leaderboard.y + leaderboard.height).toBeLessThanOrEqual((await boxOf(sections.nth(2))).y);
    });

    test("are on the home page, with a billboard above the games and a leaderboard below them", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await page.goto("/");
        const games = page.getByRole("heading", { name: "Or browse a game", level: 2 });
        await expect(games).toBeVisible();

        await expect(unit(page, "skyscraper")).toHaveCount(2);
        await expect(unit(page, "desktop_takeover")).toBeVisible();
        await expect(unit(page, "horizontal_sticky")).toBeVisible();
        await expectRailsBeside(page, ".home");

        const types = await box(page, ".home-start");
        const billboard = await box(page, "[data-fake-ad=billboard]");
        expect(billboard.y).toBeGreaterThanOrEqual(types.y + types.height);
        expect(billboard.y + billboard.height).toBeLessThanOrEqual((await boxOf(games)).y);

        const grid = await box(page, ".home-games");
        const leaderboard = await box(page, "[data-fake-ad=leaderboard]");
        expect(leaderboard.y).toBeGreaterThanOrEqual(grid.y + grid.height);
    });

    test("leave with the page, and the sign-in page has none", async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 900 });
        await openFeed(page);
        await expect(unit(page, "skyscraper")).toHaveCount(2);

        await page.getByRole("link", { name: "Sign in", exact: true }).first().click();
        await expectPage(page, "/signin");

        await expect(page.locator("[data-fake-ad]")).toHaveCount(0);
    });

    // A player new to the run writes to LeagueOfLegendsTester, whose inbox no spec reads,
    // so their own inbox has a conversation to show beside the list. The inbox fills the
    // window, so it has rails only: a sticky on its floor would cover the composer.
    test("flank the inbox where the rails fit beside it, narrowing it for them", async ({ page }) => {
        await signUp(page, "A");
        await openFeed(page, "/games/league-of-legends");
        await page.locator(".card")
            .filter({ has: page.getByRole("link", { name: "LeagueOfLegendsTester", exact: true }) })
            .locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: "LeagueOfLegendsTester" });
        await panel.getByRole("textbox", { name: "Message" }).fill("Hi, want to play tonight?");
        await panel.getByRole("button", { name: "Send" }).click();
        await expect(panel.locator(".message-own")).toHaveCount(1);
        await page.keyboard.press("Escape");

        await page.setViewportSize({ width: 1280, height: 900 });
        await page.goto("/messages");
        await expect(page.locator(".inbox-row")).toHaveCount(1);
        await expect(page.locator("[data-fake-ad]")).toHaveCount(0);

        for (const [width, rail] of [[1440, "skyscraper"], [1700, "double_mpu"]] as const) {
            await page.setViewportSize({ width, height: 900 });
            await expect(unit(page, rail)).toHaveCount(2);
            await expect(page.locator("[data-fake-ad]")).toHaveCount(2);
            await expectRailsBeside(page, ".messages-page");
            expect((await box(page, ".messages-page")).width).toBeGreaterThanOrEqual(950);
            await expect.poll(() => page.evaluate(() =>
                document.documentElement.scrollHeight <= document.documentElement.clientHeight)).toBe(true);
        }
    });
});

test.describe("the ads on a phone", () => {
    test.use({ viewport: { width: 375, height: 740 } });

    test("are a banner, a unit after the third card and a sticky, which steps aside while an overlay is open", async ({ page }) => {
        await openFeed(page);

        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(unit(page, "mobile_banner")).toBeVisible();
        await expect(unit(page, "mobile_takeover")).toBeVisible();
        await expect(page.locator("[data-fake-ad]")).toHaveCount(3);

        const summary = await box(page, ".description-summary");
        const banner = await box(page, "[data-fake-ad=mobile_banner]");
        const cards = page.locator(".card");
        expect(banner.y).toBeGreaterThanOrEqual(summary.y + summary.height);
        expect(banner.y + banner.height).toBeLessThanOrEqual((await boxOf(cards.first())).y);
        const third = await boxOf(cards.nth(2));
        const takeover = await box(page, "[data-fake-ad=mobile_takeover]");
        expect(takeover.y).toBeGreaterThanOrEqual(third.y + third.height);
        expect(takeover.y + takeover.height).toBeLessThanOrEqual((await boxOf(cards.nth(3))).y);

        await page.locator(".description-summary").click();
        await expect(page.getByRole("dialog", { name: "Tell us about you" })).toBeVisible();
        await expect(unit(page, "mobile_horizontal_sticky")).toHaveCount(0);

        await page.keyboard.press("Escape");
        await expect(page.getByRole("dialog", { name: "Tell us about you" })).toHaveCount(0);
        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
    });

    test("put a takeover after a feed's third card and an MPU after its sixth", async ({ page }) => {
        await openFeed(page, longFeedPath);

        await expectAfterCard(page, "mobile_takeover", 3);
        await expectAfterCard(page, "mobile_mpu", 6);
    });

    test("keep the banner's room before it fills, so the posts stay put when it does", async ({ page }) => {
        await page.unroute("https://hb.vntsm.com/**");
        await page.route("https://hb.vntsm.com/**", route => route.fulfill({ contentType: "text/javascript", body: "" }));
        await openFeed(page);
        const before = await boxOf(page.locator(".card").first());

        await page.addScriptTag({ content: fakeVenatus });
        await expect(unit(page, "mobile_banner")).toBeVisible();
        const after = await boxOf(page.locator(".card").first());
        expect(after.y).toBe(before.y);
    });

    test("are the sticky and two units among a guide's text", async ({ page }) => {
        await page.goto(guidePath);
        await expect(page.getByRole("heading", { name: "How to join an esports team", level: 1 })).toBeVisible();

        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(unit(page, "mobile_takeover")).toHaveCount(1);
        await expect(unit(page, "mobile_mpu")).toHaveCount(1);
        await expect(page.locator("[data-fake-ad]")).toHaveCount(3);
    });

    test("are the phone's on a tablet, which Venatus counts as one", async ({ page }) => {
        await page.setViewportSize({ width: 800, height: 1000 });
        await openFeed(page);

        await expect(unit(page, "mobile_horizontal_sticky")).toBeVisible();
        await expect(unit(page, "mobile_banner")).toBeVisible();
        await expect(unit(page, "mobile_takeover")).toBeVisible();
        await expect(units(page, desktopUnits)).toHaveCount(0);
    });
});
