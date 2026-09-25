import { expect, test } from "@playwright/test";
import { expectPage, postPath } from "../pages";

const googlebot = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)";

// `Database/Seed/Games/` seeds the ten games of the catalogue.
const handles = [
    "apex-legends", "counter-strike-2", "dota-2", "heroes-of-the-storm", "league-of-legends",
    "overwatch", "rainbow-six-siege", "team-fortress-2", "valheim", "valorant",
];

// Neither robots nor the sitemap is a page, so Caddy answers a bot with the file itself
// rather than with a render of it.
test.describe("a bot", () => {
    test.use({ userAgent: googlebot });

    test("is pointed to the sitemap by robots.txt", async ({ page, baseURL }) => {
        const response = await page.goto("/robots.txt");

        expect(response?.status()).toBe(200);
        expect(response?.headers()["content-type"]).toContain("text/plain");
        expect(await response?.text()).toContain(`Sitemap: ${baseURL}/sitemap.xml`);
    });

    // Valorant's seeded posts include GroupTester's group Night Owls, active, and
    // ExpiredTester's player post, past its 30 days.
    test("finds the home page, every feed and the active posts in the sitemap, and no expired one", async ({ page, browser, baseURL }) => {
        const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");
        const expired = await postPath(browser, baseURL!, "/games/valorant", "ExpiredTester");

        const response = await page.goto("/sitemap.xml");

        expect(response?.status()).toBe(200);
        expect(response?.headers()["content-type"]).toContain("application/xml");
        const locations = [...(await response!.text()).matchAll(/<loc>([^<]*)<\/loc>/g)].map(match => match[1]);
        expect(locations).toEqual(expect.arrayContaining([
            `${baseURL}/`,
            ...handles.map(handle => `${baseURL}/games/${handle}`),
            `${baseURL}${nightOwls}`,
        ]));
        expect(locations).not.toContain(`${baseURL}${expired}`);
    });
});

// AI crawlers run no scripts, so without a render they would read the empty shell. The
// answer engines' bots fetch a page to cite it, the training crawlers to learn the site.
// With scripts off, what the page shows is the HTML the prerenderer returned, whose links
// it has made absolute on the render origin.
const aiCrawlers = [
    ["OAI-SearchBot", "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko); compatible; OAI-SearchBot/1.0; +https://openai.com/searchbot"],
    ["ClaudeBot", "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; ClaudeBot/1.0; +claudebot@anthropic.com)"],
];

for (const [name, userAgent] of aiCrawlers)
    test.describe(name, () => {
        test.use({ userAgent, javaScriptEnabled: false });

        test("is served a game's feed prerendered", async ({ page }) => {
            test.slow();

            const response = await page.goto("/games/valorant", { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await expect(page.getByRole("link", { name: "Night Owls", exact: true })).toBeVisible();
            await expect(page.getByRole("contentinfo").getByRole("link", { name: "Privacy" })).toHaveAttribute("href", /\/privacy$/);
        });

        test("is served a post with links to the game's other posts", async ({ page, browser, baseURL }) => {
            test.slow();
            const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");

            const response = await page.goto(nightOwls, { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            const others = page.getByRole("region", { name: "Other Valorant posts" }).getByRole("link");
            await expect(others.first()).toHaveAttribute("href", /\/games\/valorant\/posts\/\d+$/);
            await expect(others.filter({ hasText: /^Night Owls$/ })).toHaveCount(0);
        });
    });

// The old site's feeds were a game's players and its teams, under handles some of which
// have changed. Each is the game's one feed now.
const oldFeeds = [
    ["/games/lol/players", "/games/league-of-legends"],
    ["/games/csgo/teams/", "/games/counter-strike-2"],
    ["/games/lol", "/games/league-of-legends"],
    ["/games/valorant/players", "/games/valorant"],
];

test.describe("an old feed's path", () => {
    for (const [oldPath, newPath] of oldFeeds)
        test(`takes a browser from ${oldPath} to ${newPath}`, async ({ page }) => {
            const response = await page.goto(oldPath);

            const redirect = await response?.request().redirectedFrom()?.response();
            expect(redirect?.status()).toBe(301);
            await expectPage(page, newPath);
        });

    // Caddy redirects before it hands anything to the prerenderer, so the bot is told where
    // the page went rather than served a render of the old path.
    for (const [oldPath, newPath] of oldFeeds)
        test(`takes a bot from ${oldPath} to ${newPath}`, async ({ request }) => {
            const response = await request.get(oldPath, { headers: { "User-Agent": googlebot }, maxRedirects: 0 });

            expect(response.status()).toBe(301);
            expect(response.headers()["location"]).toBe(newPath);
        });
});

// Search gets the pages anyone comes to read, not the forms or a player's own pages.
// Crawlers read the robots tag from the prerendered HTML.
test.describe("a page's robots tag", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    for (const [path, robots] of [
        ["/privacy", "index, follow"],
        ["/signin", "noindex"],
        ["/messages", "noindex"],
        ["/post", "noindex"],
    ])
        test(`says ${robots} to a bot at ${path}`, async ({ page }) => {
            test.slow();

            const response = await page.goto(path, { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await expect(page.locator('meta[name="robots"]')).toHaveAttribute("content", robots);
        });
});

// No page's path ends in a slash, and the router would not know one that did.
test.describe("a path with a trailing slash", () => {
    test("takes a browser to the page", async ({ page }) => {
        const response = await page.goto("/games/valorant/");

        const redirect = await response?.request().redirectedFrom()?.response();
        expect(redirect?.status()).toBe(301);
        await expectPage(page, "/games/valorant");
    });

    for (const [path, location] of [["/games/valorant/", "/games/valorant"], ["/games/valorant/?utm_source=x", "/games/valorant?utm_source=x"]])
        test(`takes a bot from ${path} to ${location}`, async ({ request }) => {
            const response = await request.get(path, { headers: { "User-Agent": googlebot }, maxRedirects: 0 });

            expect(response.status()).toBe(301);
            expect(response.headers()["location"]).toBe(location);
        });
});

// A game that left the catalogue and the old players' and teams' own pages have nothing to
// go to.
test.describe("an old path with no page of its own", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    for (const path of ["/games/splitgate/players", "/players/SomeNickname"])
        test(`is answered 404 to a bot at ${path}`, async ({ page }) => {
            test.slow();

            const response = await page.goto(path, { timeout: 60_000 });

            expect(response?.status()).toBe(404);
        });
});
