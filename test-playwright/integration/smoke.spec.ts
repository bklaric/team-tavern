import { expect, Page, test } from "@playwright/test";
import compose from "docker-compose";
import { rethrowComposeError, testStack, waitForApi } from "../stack";

// One seeded game carries every check. `stacks/test-seed/players.sql` derives the nickname
// as initcap(handle) || 'Tester', `Client/Pages/Profiles.purs` builds the title from the
// game's short title, and `Database/Seed/Games/Valorant.sql` seeds the three fields.
const game = {
    handle: "valorant",
    nickname: "ValorantTester",
    title: "Players / LFG / LFT - Valorant Team Finder | TeamTavern",
    fieldLabels: ["Rank", "Role", "Interest"],
    // One seeded profile per game, so the listing counts exactly one.
    listingCount: "Showing 1 - 1 out of 1 players",
};

const listingPath = `/games/${game.handle}/players`;

// Caddy tells the three kinds of visitor apart by what they send: the prerenderer's browser by
// its `X-RenderReady` header, a bot by its user agent. Each block below visits the same listing
// as one kind.

async function expectListingRendered(page: Page) {
    await expect(page).toHaveTitle(game.title);
    await expect(page.getByRole("link", { name: game.nickname })).toBeVisible();
    await expect(page.getByText(game.listingCount)).toBeVisible();
}

// The browser fixture carries the Desktop Chrome user agent the config sets, not headless
// Chromium's own, so this is served the site itself.
test.describe("a browser", () => {
    test("is served the player listings with the game's fields", async ({ page }) => {
        await page.goto(listingPath);

        await expectListingRendered(page);
        for (const label of game.fieldLabels)
            await expect(page.getByText(label, { exact: true }).first()).toBeVisible();
    });

    test("stays on a path the site does not have and says so", async ({ page }) => {
        await page.goto("/nopage");

        await expect(page.getByText("Page could not be found.")).toBeVisible();
        await expect(page).toHaveURL(/\/nopage$/);
    });
});

// What the prerenderer's browser sends. Caddy answers every page under it with the prerender
// shell, which boots the site and calls the API for the page's data, so the listing only
// renders if the API answers this header with JSON rather than with the shell.
test.describe("a headless browser", () => {
    test.use({ extraHTTPHeaders: { "X-RenderReady": "1" } });

    test("is served the prerender shell, which renders the player listings", async ({ page }) => {
        await page.goto(listingPath);

        // The shell is `index.html` without analytics and ads, so no ad script says it is the shell.
        await expect(page.locator('script[src*="vntsm.com"]')).toHaveCount(0);
        await expectListingRendered(page);
    });
});

// Caddy hands a bot's page request to the prerenderer. With scripts off, what the page shows
// is the HTML the prerenderer returned, not the site rendering itself again in this browser.
test.describe("a bot", () => {
    test.use({
        userAgent: "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)",
        javaScriptEnabled: false,
    });

    // The prerenderer drives a real browser against the site, which is the slowest thing in
    // the suite and slowest of all on the first render after a boot.
    test("is served the prerendered player listings", async ({ page }) => {
        test.slow();

        // The prerenderer bases the HTML on the origin it rendered, which `test.Caddyfile` sets
        // to the compose-internal `http://caddy`, so the stylesheets and images it names do not
        // resolve outside that network. Production's origin is the public site, which bots can
        // fetch; here only the content is checked.
        await page.goto(listingPath, { timeout: 60_000 });

        await expect(page).toHaveTitle(game.title);
        await expect(page.getByText(game.nickname).first()).toBeVisible();
        await expect(page.getByText(game.listingCount)).toBeVisible();
    });

    // The page names the status in a meta tag the prerenderer reads, so a crawler drops the page
    // rather than indexing the message.
    test("is answered 404 for a player who does not exist", async ({ page }) => {
        test.slow();

        const response = await page.goto("/players/NobodyTester", { timeout: 60_000 });

        expect(response?.status()).toBe(404);
        await expect(page.getByText("Player could not be found.")).toBeVisible();
    });

    test("is answered 404 for a game that does not exist", async ({ page }) => {
        test.slow();

        const response = await page.goto("/games/nogame/players", { timeout: 60_000 });

        expect(response?.status()).toBe(404);
        await expect(page.getByText("Game could not be found.")).toBeVisible();
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

    // The root is a directory, which Caddy's file matcher counts as a file, so it is the one
    // page path that has to be let through to the prerenderer by name.
    test("is served the prerendered home page", async ({ page }) => {
        test.slow();

        const response = await page.goto("/", { timeout: 60_000 });

        expect(response?.status()).toBe(200);
        await expect(page).toHaveTitle("Esports Team Finder / LFG / LFT / LFM / LFP | TeamTavern");
    });

    // A bot goes on to fetch what the prerendered HTML names, under the same user agent, so
    // Caddy has to serve those files rather than hand them to the prerenderer as pages.
    // The HTML is based on the render origin, which is the site itself in production and here
    // the compose-internal `http://caddy`, so this puts the site's origin in its place.
    test("can fetch the stylesheets and images the prerendered page names", async ({ page, baseURL }) => {
        test.slow();
        await page.route(url => url.pathname === listingPath, async route => {
            const response = await route.fetch({ timeout: 60_000 });
            const body = (await response.text()).replaceAll("http://caddy", baseURL!);
            await route.fulfill({ response, body });
        });
        const served = new Map<string, { status: number, contentType: string }>();
        page.on("response", response => {
            const url = new URL(response.url());
            if (url.origin === baseURL && url.pathname !== listingPath)
                served.set(url.pathname, { status: response.status(), contentType: response.headers()["content-type"] ?? "" });
        });

        await page.goto(listingPath, { timeout: 60_000 });

        // The prerenderer writes every link absolute. Only the site's own files count; the CDN
        // stylesheets the page also links are not Caddy's to serve.
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

        for (const path of stylesheets)
            expect(served.get(path), path).toEqual({ status: 200, contentType: expect.stringContaining("text/css") });
        for (const path of images)
            expect(served.get(path), path).toEqual({ status: 200, contentType: expect.stringContaining("image/") });

        // The logo is a 180px icon the site's stylesheet sizes down, so both arrived and applied.
        const logo = page.getByRole("img", { name: "TeamTavern logo" });
        await expect(logo).toHaveCSS("width", "26px");
        expect(await logo.evaluate(image => (image as HTMLImageElement).naturalWidth)).toBeGreaterThan(0);
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

    for (const { path, message } of [
        { path: `/players/${game.nickname}`, message: "There has been an error loading the player." },
        { path: listingPath, message: "There has been an error loading the game." },
    ])
        test(`is answered 503 for ${path}`, async ({ page }) => {
            test.slow();

            const response = await page.goto(path, { timeout: 60_000 });

            expect(response?.status()).toBe(503);
            await expect(page.getByText(message)).toBeVisible();
        });

    // The home page shows no message when its game grid fails to load, only an empty grid.
    test("is answered 503 for the home page", async ({ page }) => {
        test.slow();

        const response = await page.goto("/", { timeout: 60_000 });

        expect(response?.status()).toBe(503);
        await expect(page.getByRole("heading", { name: "Pick your game" })).toBeVisible();
        await expect(page.locator("#games .home-game")).toHaveCount(0);
    });
});
