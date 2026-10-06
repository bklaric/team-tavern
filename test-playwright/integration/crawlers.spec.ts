import { expect, test, type Page } from "@playwright/test";
import { expectPage, postPath } from "../pages";

const googlebot = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)";

// `Database/Seed/Games/` seeds the fourteen games of the catalogue.
const handles = [
    "apex-legends", "counter-strike-2", "deadlock", "dota-2", "heroes-of-the-storm", "league-of-legends",
    "marvel-rivals", "overwatch", "rainbow-six-siege", "rocket-league", "team-fortress-2", "the-finals",
    "valheim", "valorant",
];

// The site's own pages, which the footer links from every page.
const sitePages = ["/guides", "/about", "/contact", "/terms", "/privacy"];

const guidePath = "/guides/join-an-esports-team";

const guidePaths = [
    guidePath,
    "/guides/make-an-esports-team",
    "/guides/marvel-rivals-championship-team",
    "/guides/rocket-league-tournaments",
    "/guides/the-finals-ranked-with-friends",
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

    test("is kept by robots.txt from the account pages that carry a way back", async ({ page }) => {
        const response = await page.goto("/robots.txt");

        const lines = (await response!.text()).split("\n").map(line => line.trim());
        expect(lines).toEqual(expect.arrayContaining(
            ["Disallow: /signin?", "Disallow: /signup?", "Disallow: /forgot-password?"]));
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

    test("finds each guide in the sitemap dated by its last update, and the guides by the latest", async ({ page, baseURL }) => {
        const response = await page.goto("/sitemap.xml");

        const urls = new Map([...(await response!.text())
            .matchAll(/<url><loc>([^<]*)<\/loc>(?:<lastmod>([^<]*)<\/lastmod>)?<\/url>/g)]
            .map(match => [match[1], match[2]]));
        const guides = [...urls].filter(([location]) => location.startsWith(`${baseURL}/guides/`));
        expect(guides.map(([location]) => location))
            .toEqual(expect.arrayContaining(guidePaths.map(path => `${baseURL}${path}`)));
        for (const [, lastmod] of guides)
            expect(lastmod).toMatch(/^\d{4}-\d{2}-\d{2}$/);
        expect(urls.get(`${baseURL}/guides`)).toBe(guides.map(([, lastmod]) => lastmod!).sort().at(-1));
    });
});

// llms.txt and /.well-known/ are fetched by crawlers and by tools that read as a browser
// alike, and neither is a page: each gets the file, or a 404 where there is none.
for (const [who, userAgent] of [["a bot", googlebot], ["a browser", undefined]] as const)
    test.describe(who, () => {
        if (userAgent) test.use({ userAgent });

        test("is given llms.txt, linking every feed, the guides and the site's own pages", async ({ page, baseURL }) => {
            const response = await page.goto("/llms.txt");

            expect(response?.status()).toBe(200);
            expect(response?.headers()["content-type"]).toContain("text/plain");
            const text = await response!.text();
            expect(text).toMatch(/^# TeamTavern\n\n> Find players, groups and communities/);
            expect(text).toContain(`- [Valorant LFG](${baseURL}/games/valorant)\n`);
            const links = [...text.matchAll(/^- \[[^\]]+\]\(([^)]+)\)$/gm)].map(match => match[1]);
            expect(links).toEqual(expect.arrayContaining([
                ...handles.map(handle => `${baseURL}/games/${handle}`),
                ...sitePages.map(path => `${baseURL}${path}`),
                ...guidePaths.map(path => `${baseURL}${path}`),
                `${baseURL}/sitemap.xml`,
            ]));
        });

        test("finds nothing under /.well-known/", async ({ page }) => {
            const response = await page.goto("/.well-known/ai-catalog.json");

            expect(response?.status()).toBe(404);
        });
    });

// AI crawlers run no scripts, so without a render they would read the empty shell. The
// answer engines' bots fetch a page to cite it, the training crawlers to learn the site.
// With scripts off, what the page shows is the HTML the prerenderer returned, whose links
// it has made absolute on the render origin.
const aiCrawlers = [
    ["OAI-SearchBot", "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko); compatible; OAI-SearchBot/1.0; +https://openai.com/searchbot"],
    ["ClaudeBot", "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; ClaudeBot/1.0; +claudebot@anthropic.com)"],
    ["Meta-ExternalAgent", "meta-externalagent/1.1 (+https://developers.facebook.com/docs/sharing/webmasters/crawler)"],
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

        test("is served a guide prerendered, with its text", async ({ page }) => {
            test.slow();

            const response = await page.goto(guidePath, { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await expect(page.getByRole("heading", { name: "How to join an esports team", level: 1 })).toBeVisible();
            await expect(page.getByRole("table")).toBeVisible();
            await expect(page.getByRole("link", { name: "Valorant players and groups on TeamTavern" }))
                .toHaveAttribute("href", /\/games\/valorant$/);
        });
    });

// Ahrefs audits the site as a search engine would read it, so its crawler and its site
// audit are served the render search engines get, robots tag and canonical included. A
// bot Caddy has no name for is served it too, as long as it calls itself a bot, a crawler
// or a spider.
const otherBots = [
    ["AhrefsBot", "Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)"],
    ["AhrefsSiteAudit", "Mozilla/5.0 (compatible; AhrefsSiteAudit/6.1; +http://ahrefs.com/robot/site-audit)"],
    ["a bot nobody has named", "Mozilla/5.0 (compatible; UnheardOfBot/1.0; +https://example.com/bot)"],
    ["a crawler nobody has named", "UnheardOfCrawler/1.0 (+https://example.com/crawler)"],
    ["a spider nobody has named", "Mozilla/5.0 (compatible; UnheardOfSpider/1.0)"],
];

for (const [name, userAgent] of otherBots)
    test.describe(name, () => {
        test.use({ userAgent, javaScriptEnabled: false });

        test("is served a game's feed prerendered", async ({ page }) => {
            test.slow();

            const response = await page.goto("/games/valorant", { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await expect(page.getByRole("link", { name: "Night Owls", exact: true })).toBeVisible();
        });
    });

// Cubot's phones carry its name in their browser's user agent, and a player on one gets
// the site, not a render of it. With scripts off, the site is the shell, which shows no feed.
test.describe("a Cubot phone", () => {
    test.use({
        userAgent: "Mozilla/5.0 (Linux; Android 13; CUBOT NOTE 50) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Mobile Safari/537.36",
        javaScriptEnabled: false,
    });

    test("is served the site, not a render", async ({ page }) => {
        const response = await page.goto("/games/valorant");

        expect(response?.status()).toBe(200);
        await expect(page.getByRole("link", { name: "Night Owls", exact: true })).toHaveCount(0);
    });
});

// A link pasted into a chat or a post is previewed from the page a bot fetches, and a
// search engine's testing tool reads the page as its crawler does. None of them runs
// scripts, so each is served the render, whose share tags are the post's.
const previewBots = [
    ["Reddit", "Mozilla/5.0 (compatible; redditbot/1.0; +http://www.reddit.com/feedback)"],
    ["Steam chat", "Valve/Steam HTTP Client 1.0 (SteamChatURLLookup)"],
    ["Bluesky", "Mozilla/5.0 (compatible; Bluesky Cardyb/1.1; +mailto:support@bsky.app)"],
    ["Mastodon", "http.rb/5.1.1 (Mastodon/4.2.10; +https://mastodon.social/)"],
    ["Iframely", "Iframely/1.3.1 (+https://iframely.com/docs/about)"],
    ["Teams", "Mozilla/5.0 (Windows NT 6.1; WOW64) SkypeUriPreview Preview/0.5 skype-url-preview@microsoft.com"],
    ["Google-InspectionTool", "Mozilla/5.0 (compatible; Google-InspectionTool/1.0)"],
    ["DuckDuckBot", "DuckDuckBot/1.1; (+http://duckduckgo.com/duckduckbot.html)"],
];

for (const [name, userAgent] of previewBots)
    test.describe(name, () => {
        test.use({ userAgent, javaScriptEnabled: false });

        test("is served a post prerendered, with its share tags", async ({ page, browser, baseURL }) => {
            test.slow();
            const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");

            const response = await page.goto(nightOwls, { timeout: 60_000 });

            expect(response?.status()).toBe(200);
            await expect(page.locator('meta[property="og:title"]'))
                .toHaveAttribute("content", "Night Owls · Valorant group | TeamTavern");
            await expect(page.locator('meta[property="og:image"]'))
                .toHaveAttribute("content", /\/images\/games\/valorant\.webp$/);
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
        [guidePath, "index, follow"],
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

    test("leads from the home page to the guides", async ({ page }) => {
        test.slow();

        await page.goto("/guides", { timeout: 60_000 });

        expect(trail(await structuredData(page))).toEqual([
            { position: 1, name: "Home", path: "/" },
            { position: 2, name: "Guides", path: "/guides" },
        ]);
    });

    test("names TeamTavern as a guide's author and publisher, and leads to it through the guides", async ({ page }) => {
        test.slow();

        await page.goto(guidePath, { timeout: 60_000 });

        const { "@context": context, "@graph": graph } = await structuredData(page);
        expect(context).toBe("https://schema.org");
        const node = (type: string) => graph.find((node: any) => node["@type"] === type);
        const organization = node("Organization");
        expect(organization).toMatchObject({ name: "TeamTavern" });
        expect(node("Article")).toMatchObject({
            headline: "How to join an esports team",
            image: expect.stringMatching(/\/logo-512\.png$/),
            datePublished: expect.stringMatching(/^\d{4}-\d{2}-\d{2}$/),
            dateModified: expect.stringMatching(/^\d{4}-\d{2}-\d{2}$/),
            author: { "@id": organization["@id"] },
            publisher: { "@id": organization["@id"] },
        });
        expect(new URL(node("Article").mainEntityOfPage).pathname).toBe(guidePath);
        expect(trail(node("BreadcrumbList"))).toEqual([
            { position: 1, name: "Home", path: "/" },
            { position: 2, name: "Guides", path: "/guides" },
            { position: 3, name: "How to join an esports team", path: guidePath },
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

// A shared link shows the game's cover for a feed or a post, and the logo for the rest.
test.describe("a page's share image", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    type Image = { path: string, alt: string, width: string, height: string, type: string };

    const cover: Image =
        { path: "/images/games/valorant.webp", alt: "Valorant cover", width: "600", height: "900", type: "image/webp" };

    const expectImage = async (page: Page, image: Image) => {
        for (const name of ["og:image", "twitter:image"]) {
            const content = await page.locator(`meta[property="${name}"], meta[name="${name}"]`).getAttribute("content");
            expect(new URL(content!).pathname).toBe(image.path);
        }
        await expect(page.locator('meta[property="og:image:alt"]')).toHaveAttribute("content", image.alt);
        await expect(page.locator('meta[name="twitter:image:alt"]')).toHaveAttribute("content", image.alt);
        await expect(page.locator('meta[property="og:image:width"]')).toHaveAttribute("content", image.width);
        await expect(page.locator('meta[property="og:image:height"]')).toHaveAttribute("content", image.height);
        await expect(page.locator('meta[property="og:image:type"]')).toHaveAttribute("content", image.type);
    };

    test("is the game's cover on its feed and its posts", async ({ page, browser, baseURL }) => {
        test.slow();
        const nightOwls = await postPath(browser, baseURL!, "/games/valorant", "Night Owls");

        await page.goto("/games/valorant", { timeout: 60_000 });
        await expectImage(page, cover);

        await page.goto(nightOwls, { timeout: 60_000 });
        await expectImage(page, cover);
    });

    test("is the logo on a page of the site's own", async ({ page }) => {
        test.slow();

        await page.goto("/about", { timeout: 60_000 });

        await expectImage(page,
            { path: "/logo-512.png", alt: "TeamTavern logo", width: "512", height: "512", type: "image/png" });
    });
});

test("a page drops the share image of the game before it", async ({ page }) => {
    await page.goto("/games/valorant");
    await expect(page.locator('meta[property="og:image"]')).toHaveAttribute("content", /\/images\/games\/valorant\.webp$/);

    await page.getByRole("contentinfo").getByRole("link", { name: "About" }).click();

    await expectPage(page, "/about");
    await expect(page.locator('meta[property="og:image"]')).toHaveAttribute("content", /\/logo-512\.png$/);
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

test.describe("a guide that doesn't exist", () => {
    test.use({ userAgent: googlebot, javaScriptEnabled: false });

    test("is answered 404 to a bot", async ({ page }) => {
        test.slow();

        const response = await page.goto("/guides/no-such-guide", { timeout: 60_000 });

        expect(response?.status()).toBe(404);
    });
});
