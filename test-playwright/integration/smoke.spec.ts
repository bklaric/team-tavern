import { expect, Page, test } from "@playwright/test";
import compose from "docker-compose";
import { rethrowComposeError, testStack, waitForApi } from "../stack";

// The home page loads its cover grid from `/api/games`, so it is the page that shows whether
// the site and the API both answer. `Database/Seed/Games/` seeds ten games.
const home = { path: "/", title: "TeamTavern", gameCount: 10 };

// Caddy tells the three kinds of visitor apart by what they send: the prerenderer's browser by
// its `X-RenderReady` header, a bot by its user agent. Each block below visits the home page
// as one kind.

async function expectHomeRendered(page: Page) {
    await expect(page).toHaveTitle(home.title);
    await expect(page.locator(".cover-grid .cover")).toHaveCount(home.gameCount);
}

// The browser fixture carries the Desktop Chrome user agent the config sets, not headless
// Chromium's own, so this is served the site itself.
test.describe("a browser", () => {
    test("is served the home page with the game covers", async ({ page }) => {
        await page.goto(home.path);

        await expectHomeRendered(page);
    });

    // Caddy answers the redirect itself; the ad network's file is not the suite's to fetch.
    test("is redirected from ads.txt to the ad network's", async ({ page }) => {
        const adsTxt = "https://adstxt.venatusmedia.com/teamtavern.net/ads.txt";
        await page.route(adsTxt, route => route.fulfill({ contentType: "text/plain", body: "" }));

        const response = await page.goto("/ads.txt");

        const redirect = await response?.request().redirectedFrom()?.response();
        expect(redirect?.status()).toBe(301);
        expect(page.url()).toBe(adsTxt);
    });

    test("stays on a path the site does not have and says so", async ({ page }) => {
        await page.goto("/nopage");

        await expect(page.getByText("Page could not be found.")).toBeVisible();
        await expect(page).toHaveURL(/\/nopage$/);
    });
});

// What the prerenderer's browser sends. Caddy answers every page under it with the prerender
// shell, which boots the site and calls the API for the page's data, so the page only
// renders if the API answers this header with JSON rather than with the shell.
test.describe("a headless browser", () => {
    test.use({ extraHTTPHeaders: { "X-RenderReady": "1" } });

    test("is served the prerender shell, which renders the home page", async ({ page }) => {
        await page.goto(home.path);

        // The shell is `index.html` without ads, so no ad script says it is the shell.
        await expect(page.locator('script[src*="vntsm.com"]')).toHaveCount(0);
        await expectHomeRendered(page);
    });
});

// Caddy hands a bot's page request to the prerenderer. With scripts off, what the page shows
// is the HTML the prerenderer returned, not the site rendering itself again in this browser.
test.describe("a bot", () => {
    test.use({
        userAgent: "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)",
        javaScriptEnabled: false,
    });

    // The root is a directory, which Caddy's file matcher counts as a file, so it is the one
    // page path that has to be let through to the prerenderer by name. The prerenderer drives
    // a real browser against the site, which is the slowest thing in the suite and slowest of
    // all on the first render after a boot.
    test("is served the prerendered home page", async ({ page }) => {
        test.slow();

        const response = await page.goto(home.path, { timeout: 60_000 });

        expect(response?.status()).toBe(200);
        await expectHomeRendered(page);
    });

    // Caddy answers a missing image itself rather than rendering it as a page.
    test("is answered 404 for an image that does not exist, without a render", async ({ page }) => {
        const response = await page.goto("/images/games/nogame.webp");

        expect(response?.status()).toBe(404);
        expect(response?.headers()["content-type"] ?? "").not.toContain("text/html");
    });

    // Caddy knows no routes: every path without a file behind it is rendered, and the router
    // declares the 404 for one it does not know, whether or not the path looks like a file.
    for (const path of ["/nopage", "/nopage.txt"])
        test(`is answered 404 for ${path}, which is neither a page nor a file`, async ({ page }) => {
            test.slow();

            const response = await page.goto(path, { timeout: 60_000 });

            expect(response?.status()).toBe(404);
            await expect(page.getByText("Page could not be found.")).toBeVisible();
        });

    // A bot goes on to fetch what the prerendered HTML names, under the same user agent, so
    // Caddy has to serve those files rather than hand them to the prerenderer as pages.
    // The HTML is based on the render origin, which is the site itself in production and here
    // the compose-internal `http://caddy`, so this puts the site's origin in its place.
    test("can fetch the stylesheets and images the prerendered page names", async ({ page, baseURL }) => {
        test.slow();
        await page.route(url => url.pathname === home.path, async route => {
            const response = await route.fetch({ timeout: 60_000 });
            const body = (await response.text()).replaceAll("http://caddy", baseURL!);
            await route.fulfill({ response, body });
        });
        const served = new Map<string, { status: number, contentType: string }>();
        page.on("response", response => {
            const url = new URL(response.url());
            if (url.origin === baseURL && url.pathname !== home.path)
                served.set(url.pathname, { status: response.status(), contentType: response.headers()["content-type"] ?? "" });
        });

        await page.goto(home.path, { timeout: 60_000 });

        // The prerenderer writes every link absolute.
        const paths = async (selector: string, attribute: string) =>
            (await page.locator(selector).evaluateAll((elements, attribute) =>
                elements.map(element => element.getAttribute(attribute) ?? ""), attribute))
            .map(link => new URL(link, baseURL))
            .filter(url => url.origin === baseURL)
            .map(url => url.pathname);
        const stylesheets = await paths('link[rel="stylesheet"]', "href");
        const images = await paths("img", "src");
        expect(stylesheets.length).toBeGreaterThan(0);
        expect(images.length).toBeGreaterThan(0);

        for (const image of await page.locator("img").all())
            await image.scrollIntoViewIfNeeded();
        for (const path of stylesheets)
            await expect.poll(() => served.get(path), path)
                .toEqual({ status: 200, contentType: expect.stringContaining("text/css") });
        for (const path of images)
            await expect.poll(() => served.get(path), path)
                .toEqual({ status: 200, contentType: expect.stringContaining("image/") });
    });
});

// With the API stopped, a page's own request for its data fails, and the page names a 503 for
// the prerenderer to answer with, so a crawler keeps what it has instead of indexing the error.
// The API comes back up before the block ends, whether its tests passed or not.
test.describe("a bot, while the API is down", () => {
    test.use({
        userAgent: "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)",
        javaScriptEnabled: false,
    });

    test.beforeAll(async () => {
        await rethrowComposeError(() => compose.stopOne("node", testStack));
    });

    test.afterAll(async ({ request }) => {
        // The node container reinstalls its dependencies on every start.
        test.setTimeout(120_000);
        await rethrowComposeError(() => compose.upOne("node", testStack));
        await waitForApi(request);
    });

    test("is answered 503 for the home page", async ({ page }) => {
        test.slow();

        const response = await page.goto(home.path, { timeout: 60_000 });

        expect(response?.status()).toBe(503);
        await expect(page.getByText("There has been an error loading the games.")).toBeVisible();
        await expect(page.locator(".cover")).toHaveCount(0);
    });
});
