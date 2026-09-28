import { expect, test, type Page } from "@playwright/test";
import { expectPage, postPath } from "../pages";

const googlebot = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)";

// `Database/Seed/Games/` seeds the ten games of the catalogue.
const handles = [
    "apex-legends", "counter-strike-2", "dota-2", "heroes-of-the-storm", "league-of-legends",
    "overwatch", "rainbow-six-siege", "team-fortress-2", "valheim", "valorant",
];

// The site's own pages, which the footer links from every page.
const sitePages = ["/about", "/contact", "/terms", "/privacy"];

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
    test("finds the home page, the site's own pages, every feed and the active posts in the sitemap, and no expired one", async ({ page, browser, baseURL }) => {
        const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");
        const expired = await postPath(browser, baseURL!, "/games/valorant", "ExpiredTester");

        const response = await page.goto("/sitemap.xml");

        expect(response?.status()).toBe(200);
        expect(response?.headers()["content-type"]).toContain("application/xml");
        const locations = [...(await response!.text()).matchAll(/<loc>([^<]*)<\/loc>/g)].map(match => match[1]);
        expect(locations).toEqual(expect.arrayContaining([
            `${baseURL}/`,
            ...sitePages.map(path => `${baseURL}${path}`),
            ...handles.map(handle => `${baseURL}/games/${handle}`),
            `${baseURL}${nightOwls}`,
        ]));
        expect(locations).not.toContain(`${baseURL}${expired}`);
    });

    // ValorantTester's seeded post is active, and says only "Seeded player post for Valorant."
    test("finds no post that says too little, and dates the home page and a feed by their newest post", async ({ page, browser, baseURL }) => {
        const thin = await postPath(browser, baseURL!, "/games/valorant", "ValorantTester");

        const response = await page.goto("/sitemap.xml");

        const urls = new Map([...(await response!.text())
            .matchAll(/<url><loc>([^<]*)<\/loc>(?:<lastmod>([^<]*)<\/lastmod>)?<\/url>/g)]
            .map(match => [match[1], match[2]]));
        expect(urls.has(`${baseURL}${thin}`)).toBe(false);
        const posts = [...urls].filter(([location]) => location.includes("/posts/"));
        const newest = (prefix: string) =>
            posts.filter(([location]) => location.startsWith(prefix)).map(([, lastmod]) => lastmod!).sort().at(-1);
        expect(urls.get(`${baseURL}/`)).toBe(newest(`${baseURL}/games/`));
        expect(urls.get(`${baseURL}/games/valorant`)).toBe(newest(`${baseURL}/games/valorant/`));
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
            for (const path of sitePages)
                await expect(page.getByRole("contentinfo").locator(`a[href$="${path}"]`)).toHaveCount(1);
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
        ...sitePages.map(path => [path, "index, follow"]),
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

// Search engines read a page's JSON-LD from the prerendered HTML. Its addresses are on the
// origin the page was rendered at, so they are matched on their paths.
test.describe("a page's structured data", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    const structuredData = (page: Page) =>
        page.locator('script[type="application/ld+json"]').textContent().then(text => JSON.parse(text!));

    const trail = (breadcrumbs: any) =>
        breadcrumbs.itemListElement.map((crumb: any) =>
            ({ position: crumb.position, name: crumb.name, path: new URL(crumb.item).pathname }));

    test("names the site and who runs it on the home page", async ({ page }) => {
        test.slow();

        await page.goto("/", { timeout: 60_000 });

        const { "@context": context, "@graph": [organization, website] } = await structuredData(page);
        expect(context).toBe("https://schema.org");
        expect(organization).toMatchObject({ "@type": "Organization", name: "TeamTavern" });
        expect(new URL(organization.logo).pathname).toBe("/logo-512.png");
        expect(website).toMatchObject({ "@type": "WebSite", name: "TeamTavern", publisher: { "@id": organization["@id"] } });
    });

    test("leads from the home page to a game's feed", async ({ page }) => {
        test.slow();

        await page.goto("/games/valorant", { timeout: 60_000 });

        const breadcrumbs = await structuredData(page);
        expect(breadcrumbs["@type"]).toBe("BreadcrumbList");
        expect(trail(breadcrumbs)).toEqual([
            { position: 1, name: "Home", path: "/" },
            { position: 2, name: "Valorant", path: "/games/valorant" },
        ]);
    });

    test("leads from the home page through the game's feed to a post", async ({ page, browser, baseURL }) => {
        test.slow();
        const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");

        await page.goto(nightOwls, { timeout: 60_000 });

        expect(trail(await structuredData(page))).toEqual([
            { position: 1, name: "Home", path: "/" },
            { position: 2, name: "Valorant", path: "/games/valorant" },
            { position: 3, name: "Night Owls · Valorant group", path: nightOwls },
        ]);
    });

    test("is left out of a page with nothing to say", async ({ page }) => {
        test.slow();

        await page.goto("/about", { timeout: 60_000 });

        await expect(page.getByRole("heading", { level: 1 })).toBeVisible();
        await expect(page.locator('script[type="application/ld+json"]')).toHaveCount(0);
    });
});

// The site moves between pages without a reload, so the page left takes its JSON-LD with it.
test("a page drops the structured data of the page before it", async ({ page }) => {
    await page.goto("/");
    await expect(page.locator('script[type="application/ld+json"]')).toHaveCount(1);

    await page.getByRole("contentinfo").getByRole("link", { name: "About" }).click();

    await expectPage(page, "/about");
    await expect(page.locator('script[type="application/ld+json"]')).toHaveCount(0);
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
