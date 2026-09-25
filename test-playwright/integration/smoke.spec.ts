import { expect, Page, test } from "@playwright/test";
import compose from "docker-compose";
import { expectPage, postPath } from "../pages";
import { rethrowComposeError, testStack, waitForApi } from "../stack";

const googlebot = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)";

// The home page loads its cover grid from `/api/games`, so it is the page that shows whether
// the site and the API both answer. `Database/Seed/Games/` seeds ten games.
const home = { path: "/", title: "TeamTavern: find players, groups and communities", gameCount: 10 };

// Valorant's seeded posts (`stacks/test-seed/players.sql`) include GroupTester's group Night
// Owls, active, and ExpiredTester's player post, past its 30 days. Their paths are read off
// the feed once, before anything below runs, since the API is stopped in the last block.
const feedPath = "/games/valorant";
let nightOwlsPath: string;
let expiredPath: string;

test.beforeAll(async ({ browser }, testInfo) => {
    const baseURL = testInfo.project.use.baseURL!;
    nightOwlsPath = await postPath(browser, baseURL, feedPath, "Night Owls");
    expiredPath = await postPath(browser, baseURL, feedPath, "ExpiredTester");
});

async function expectHomeRendered(page: Page) {
    await expect(page).toHaveTitle(home.title);
    await expect(page.locator(".cover-grid .cover")).toHaveCount(home.gameCount);
}

// Each kind of page the site serves, with what shows that it rendered and what it says when
// the API doesn't answer.
const pageKinds = [
    {
        name: "the home page",
        path: () => home.path,
        expectRendered: expectHomeRendered,
        failure: "There has been an error loading the games.",
    },
    {
        name: "a game's feed",
        path: () => feedPath,
        expectRendered: async (page: Page) => {
            await expect(page).toHaveTitle("Valorant: find players, groups and communities | TeamTavern");
            await expect(page.getByRole("link", { name: "Night Owls", exact: true })).toBeVisible();
        },
        failure: "There has been an error loading the game.",
    },
    {
        name: "a post's page",
        path: () => nightOwlsPath,
        expectRendered: async (page: Page) => {
            await expect(page).toHaveTitle("Night Owls · Valorant group | TeamTavern");
            await expect(page.getByRole("heading", { name: "Night Owls", level: 1 })).toBeVisible();
        },
        failure: "There has been an error loading the post.",
    },
];

// Caddy tells the three kinds of visitor apart by what they send: the prerenderer's browser by
// its `X-RenderReady` header, a bot by its user agent. Each block below visits the pages as
// one kind.

// The browser fixture carries the Desktop Chrome user agent the config sets, not headless
// Chromium's own, so this is served the site itself.
test.describe("a browser", () => {
    for (const kind of pageKinds)
        test(`is served ${kind.name}`, async ({ page }) => {
            await page.goto(kind.path());

            await kind.expectRendered(page);
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

    // The home page is the list of games, so there is no page of its own at `/games`.
    for (const path of ["/games", "/games/"])
        test(`is redirected from ${path} to the home page`, async ({ page }) => {
            const response = await page.goto(path);

            const redirect = await response?.request().redirectedFrom()?.response();
            expect(redirect?.status()).toBe(301);
            await expectPage(page, home.path);
            await expectHomeRendered(page);
        });

    // Every build names its script and stylesheet anew, and the shell names the build's. The
    // root is a directory rather than a path rewritten to the shell, so it is a kind of its own.
    for (const kind of pageKinds)
        test(`keeps the build's files for good and asks again for the shell of ${kind.name}`, async ({ page }) => {
            const cacheControl = new Map<string, string>();
            page.on("response", response =>
                cacheControl.set(new URL(response.url()).pathname, response.headers()["cache-control"] ?? ""));

            const shell = await page.goto(kind.path());
            await kind.expectRendered(page);

            expect(shell?.headers()["cache-control"]).toBe("no-cache");
            const build = [...cacheControl].filter(([path]) => /^\/(app|style)\.min\./.test(path));
            expect(build.map(([path]) => path.split(".")[0])).toEqual(expect.arrayContaining(["/app", "/style"]));
            for (const [path, value] of build)
                expect(value, path).toBe("public, max-age=31536000, immutable");
            expect(cacheControl.get("/fonts/inter.css")).toBe("public, max-age=604800");
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

    for (const kind of pageKinds)
        test(`is served the prerender shell, which renders ${kind.name}`, async ({ page }) => {
            await page.goto(kind.path());

            // The shell is `index.html` without ads, so no ad script says it is the shell.
            await expect(page.locator('script[src*="vntsm.com"]')).toHaveCount(0);
            await kind.expectRendered(page);
        });
});

// Caddy hands a bot's page request to the prerenderer. With scripts off, what the page shows
// is the HTML the prerenderer returned, not the site rendering itself again in this browser.
// The prerenderer drives a real browser against the site, which is the slowest thing in the
// suite and slowest of all on the first render after a boot.
test.describe("a bot", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    // The root is a directory, which Caddy's file matcher counts as a file, so it is the one
    // page path that has to be let through to the prerenderer by name.
    for (const kind of pageKinds)
        test(`is served ${kind.name} prerendered`, async ({ page }) => {
            test.slow();

            const response = await page.goto(kind.path(), { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await kind.expectRendered(page);
        });

    // Crawlers read the robots tag from the prerendered HTML.
    test("is told to leave an expired post out of its index, and to index an active one", async ({ page }) => {
        test.slow();

        await page.goto(expiredPath, { timeout: 60_000 });
        await expect(page.locator('meta[name="robots"]')).toHaveAttribute("content", "noindex");

        await page.goto(nightOwlsPath, { timeout: 60_000 });
        await expect(page.locator('meta[name="robots"]')).toHaveAttribute("content", "index, follow");
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

    test("is answered 404 for a post that isn't there", async ({ page }) => {
        test.slow();

        const response = await page.goto(`${feedPath}/posts/999999`, { timeout: 60_000 });

        expect(response?.status()).toBe(404);
        await expect(page.getByRole("heading", { name: "This post is gone" })).toBeVisible();
    });

    // A bot goes on to fetch what the prerendered HTML names, under the same user agent, so
    // Caddy has to serve those files rather than hand them to the prerenderer as pages.
    // The HTML is based on the render origin, which is the site itself in production and here
    // the compose-internal `http://tt-caddy`, so this puts the site's origin in its place.
    for (const kind of pageKinds)
        test(`can fetch the stylesheets and images ${kind.name} names`, async ({ page, baseURL }) => {
            test.slow();
            const path = kind.path();
            await page.route(url => url.pathname === path, async route => {
                const response = await route.fetch({ timeout: 60_000 });
                const body = (await response.text()).replaceAll("http://tt-caddy", baseURL!);
                await route.fulfill({ response, body });
            });
            const served = new Map<string, { status: number, contentType: string }>();
            page.on("response", response => {
                const url = new URL(response.url());
                if (url.origin === baseURL && url.pathname !== path)
                    served.set(url.pathname, { status: response.status(), contentType: response.headers()["content-type"] ?? "" });
            });

            await page.goto(path, { timeout: 60_000 });

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
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    test.beforeAll(async () => {
        await rethrowComposeError(() => compose.stopOne("tt-node", testStack));
    });

    test.afterAll(async ({ request }) => {
        // The node container reinstalls its dependencies on every start.
        test.setTimeout(120_000);
        await rethrowComposeError(() => compose.upOne("tt-node", testStack));
        await waitForApi(request);
    });

    for (const kind of pageKinds)
        test(`is answered 503 for ${kind.name}`, async ({ page }) => {
            test.slow();

            const response = await page.goto(kind.path(), { timeout: 60_000 });

            expect(response?.status()).toBe(503);
            await expect(page.getByText(kind.failure)).toBeVisible();
        });
});
