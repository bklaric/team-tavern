import { expect, test } from "@playwright/test";

// One seeded game carries every check. `stacks/test-seed/players.sql` derives the nickname
// as initcap(handle) || 'Tester', `Client/Pages/Profiles.purs` builds the title from the
// game's short title, and `Database/Seed/Games/Valorant.sql` seeds the three fields, which
// `Server/Game/ViewGame.purs` returns ordered by ordinal.
const game = {
    handle: "valorant",
    nickname: "ValorantTester",
    title: "Players / LFG / LFT - Valorant Team Finder | TeamTavern",
    fields: [
        { key: "rank", label: "Rank" },
        { key: "role", label: "Role" },
        { key: "interest", label: "Interest" },
    ],
    // One seeded profile per game, so the listing counts exactly one.
    listingCount: "Showing 1 - 1 out of 1 players",
};

// What the prerenderer's browser sends. It calls the API for the page it is rendering, so
// Caddy has to let these through to the API rather than answering them with the prerender
// shell it serves every other path under this user agent.
const headlessChromeUserAgent =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"
    + " HeadlessChrome/153.0.8010.12 Safari/537.36";

const googlebotUserAgent =
    "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)";

type ViewGameResponse = {
    handle: string;
    fields: { key: string, label: string }[];
};

test("the API answers a headless browser with JSON", async ({ request }) => {
    const response = await request.get(`/api/games/${game.handle}`, {
        headers: { "user-agent": headlessChromeUserAgent },
    });

    expect(response.status()).toBe(200);
    expect(response.headers()["content-type"]).toContain("application/json");
    expect(((await response.json()) as ViewGameResponse).handle).toBe(game.handle);
});

// The prerenderer drives a real browser against the site, which is the slowest thing in
// the suite and slowest of all on the first render after a boot.
test("a bot is served the prerendered player listings", async ({ request }) => {
    test.slow();

    const response = await request.get(`/games/${game.handle}/players`, {
        headers: { "user-agent": googlebotUserAgent },
        timeout: 60_000,
    });

    expect(response.status()).toBe(200);

    // Only page content. The render origin is the compose-internal `http://caddy`, so every
    // URL in this HTML names a host that exists only inside that network.
    const body = await response.text();
    expect(body).toContain(`<title>${game.title}</title>`);
    expect(body).toContain(game.listingCount);
    expect(body).toContain(game.nickname);
});

// The browser fixture carries the Desktop Chrome user agent the config sets, not headless
// Chromium's own, so this asks for the site rather than the prerender shell.
test("a browser renders the player listings", async ({ page }) => {
    await page.goto(`/games/${game.handle}/players`);

    await expect(page.getByRole("link", { name: game.nickname })).toBeVisible();
});

test("the API answers a browser with the game's fields", async ({ request }) => {
    const response = await request.get(`/api/games/${game.handle}`);

    expect(response.status()).toBe(200);
    const body = (await response.json()) as ViewGameResponse;
    expect(body.handle).toBe(game.handle);
    expect(body.fields.map(({ key, label }) => ({ key, label }))).toEqual(game.fields);
});
