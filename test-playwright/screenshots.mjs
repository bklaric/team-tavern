// Screenshots every screen of the site at phone and desktop width, against the running
// test stack, for comparing with the prototype's (redesign/prototype/screenshot.mjs):
//   "$(volta which node)" test-playwright/screenshots.mjs [--only=<text>] ...
// With no --only it takes every scene; each --only keeps the scenes whose name holds its
// text. The shots go to test-playwright/screenshots/. Scenes sign in as the seeded players
// (stacks/test-seed/players.sql) and make what the seed doesn't hold, a conversation or a
// block, through the pages, so run it on a stack `npm test` is about to reseed anyway.
// Errors the pages throw or log are printed.
import { chromium } from "playwright";
import { mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const origin = "http://localhost:8080";
const password = "tester-password";
const widths = { phone: 375, desktop: 1280 };
const only = process.argv.slice(2)
    .filter(arg => arg.startsWith("--only="))
    .map(arg => arg.slice("--only=".length));

try {
    const response = await fetch(`${origin}/api/games`);
    if (!response.ok) {
        throw new Error(`/api/games answered ${response.status}`);
    }
} catch (error) {
    console.error(`The test stack doesn't answer at ${origin} (${error.message}).`);
    console.error("Build with ./build.sh and run npm test, or bring the stack up with");
    console.error("docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml up -d");
    process.exit(1);
}

let suffixes = 0;
const unique = prefix => `${prefix}${Date.now().toString(36)}${++suffixes}`;

// The router marks the page it has drawn with its path; a list that is still fetching is
// aria-busy; covers and fonts load on their own. The ad script keeps the network busy, so
// the images are waited on rather than the network.
async function settle(page) {
    await page.locator("#spa-teamtavern > [data-path]").waitFor();
    await page.waitForFunction(() =>
        !document.querySelector("[aria-busy=true]") && [...document.images].every(image => image.complete));
    await page.evaluate(() => document.fonts.ready);
}

async function open(page, path) {
    await page.goto(path);
    await settle(page);
}

async function signIn(page, email) {
    await page.goto("/signin");
    await page.getByLabel("Email or nickname").fill(email);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await page.getByRole("button", { name: "Account menu" }).waitFor();
}

async function signUp(page, prefix) {
    const nickname = unique(prefix);
    await page.goto("/signup");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await page.getByRole("button", { name: "Account menu" }).waitFor();
    return nickname;
}

const card = (page, name) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openPanel(page, handle, name) {
    await open(page, `/games/${handle}`);
    await card(page, name).locator(".card-contact").click();
    const panel = page.getByRole("dialog", { name });
    await panel.waitFor();
    await settle(page);
    return panel;
}

async function openPost(page, handle, name) {
    await open(page, `/games/${handle}`);
    await card(page, name).getByRole("link", { name, exact: true }).click();
    await settle(page);
}

// Discord's part in signing up, as test-playwright/discord.ts plays it: straight back to
// the page with a token the test stack's stub reads as a new user.
async function fakeDiscord(page) {
    const id = unique("");
    await page.route(url => url.hostname === "discord.com" && url.pathname === "/api/oauth2/authorize", route => {
        const authorize = new URL(route.request().url());
        const user = { id, username: `d${id}`, discriminator: "0", email: null, verified: false };
        const token = encodeURIComponent(JSON.stringify(user));
        const state = encodeURIComponent(authorize.searchParams.get("state") ?? "");
        return route.fulfill({
            status: 302,
            headers: {
                location: `${authorize.searchParams.get("redirect_uri")}#token_type=Bearer&access_token=${token}&state=${state}`,
            },
        });
    });
}

// Each scene runs in a browser of its own at each width it names, signed in as `as` where
// it has one, and calls `shot` for every picture it takes.
const scenes = [
    { name: "home-signed-out", run: async (page, shot) => { await open(page, "/"); await shot(); } },
    { name: "home-without-posts", as: "new@example.com", run: async (page, shot) => { await open(page, "/"); await shot(); } },
    { name: "home-owner", as: "owner@example.com", run: async (page, shot) => { await open(page, "/"); await shot(); } },
    {
        name: "header-games", as: "owner@example.com", run: async (page, shot) => {
            await open(page, "/");
            await page.getByRole("button", { name: "Games" }).click();
            await settle(page);
            await shot();
        },
    },
    {
        name: "header-notifications", as: "owner@example.com", run: async (page, shot) => {
            await open(page, "/");
            await page.getByRole("button", { name: /^Notifications/ }).click();
            await page.locator(".notification").first().waitFor();
            await shot();
        },
    },
    {
        name: "header-account", as: "owner@example.com", run: async (page, shot) => {
            await open(page, "/");
            await page.getByRole("button", { name: "Account menu" }).click();
            await shot();
        },
    },
    {
        name: "header-menu-signed-out", devices: ["phone"], run: async (page, shot) => {
            await open(page, "/");
            await page.getByRole("button", { name: "Menu" }).click();
            await shot();
        },
    },
    { name: "feed", run: async (page, shot) => { await open(page, "/games/valorant"); await shot(); } },
    {
        name: "feed-description", run: async (page, shot, device) => {
            await open(page, "/games/valorant");
            if (device === "phone") {
                await page.locator(".description-summary").click();
            } else {
                await page.getByRole("button", { name: "Rank", exact: true }).click();
            }
            await settle(page);
            await shot();
        },
    },
    {
        name: "feed-described", as: "valorant@example.com", run: async (page, shot) => {
            await open(page, "/games/valorant");
            await shot();
        },
    },
    {
        name: "feed-details", run: async (page, shot) => {
            await open(page, "/games/valorant");
            await card(page, "Night Owls").getByRole("button", { name: "Details" }).click();
            await page.waitForTimeout(300);
            await shot();
        },
    },
    { name: "post-page", run: async (page, shot) => { await openPost(page, "valorant", "Night Owls"); await shot(); } },
    {
        // The feed leaves out the viewer's own posts, so the owner goes from their home page.
        name: "post-page-owner", as: "group@example.com", run: async (page, shot) => {
            await open(page, "/");
            await page.getByRole("link", { name: "Night Owls", exact: true }).click();
            await settle(page);
            await shot();
        },
    },
    { name: "post-page-expired", run: async (page, shot) => { await openPost(page, "valorant", "ExpiredTester"); await shot(); } },
    { name: "post-page-gone", run: async (page, shot) => { await open(page, "/games/valorant/posts/999999"); await shot(); } },
    {
        name: "panel-message", as: "new@example.com", run: async (page, shot) => {
            await openPanel(page, "valorant", "ValorantTester");
            await shot();
        },
    },
    {
        name: "panel-contacts", as: "new@example.com", run: async (page, shot) => {
            await openPanel(page, "team-fortress-2", "TeamFortress2Tester");
            await shot();
        },
    },
    {
        name: "panel-community", as: "new@example.com", run: async (page, shot) => {
            await openPanel(page, "valorant", "Radiant Rising");
            await shot();
        },
    },
    {
        name: "panel-signed-out", run: async (page, shot) => {
            await open(page, "/games/valorant");
            await card(page, "ValorantTester").locator(".card-contact").click();
            await settle(page);
            await shot();
        },
    },
    { name: "post-type", as: "new@example.com", run: async (page, shot) => { await open(page, "/post"); await shot(); } },
    { name: "post-game", as: "new@example.com", run: async (page, shot) => { await open(page, "/post/group"); await shot(); } },
    {
        name: "post-screen", as: "new@example.com", run: async (page, shot) => {
            await open(page, "/games/valorant/post/group");
            await shot();
        },
    },
    {
        name: "post-preview", as: "new@example.com", devices: ["phone"], run: async (page, shot) => {
            await open(page, "/games/valorant/post/player");
            await page.getByRole("button", { name: "Preview" }).click();
            await shot();
        },
    },
    {
        name: "post-existing", as: "group@example.com", run: async (page, shot) => {
            await open(page, "/games/valorant/post/group");
            await shot();
            await page.getByRole("button", { name: "Delete it" }).click();
            await shot("delete");
        },
    },
    {
        name: "post-matches", as: "group@example.com", run: async (page, shot) => {
            await open(page, "/games/valorant/post/group/live");
            await shot();
        },
    },
    { name: "sign-up", run: async (page, shot) => { await open(page, "/signup"); await shot(); } },
    { name: "sign-in", run: async (page, shot) => { await open(page, "/signin"); await shot(); } },
    { name: "forgot-password", run: async (page, shot) => { await open(page, "/forgot-password"); await shot(); } },
    {
        name: "nickname", run: async (page, shot) => {
            await fakeDiscord(page);
            await open(page, "/signup");
            await page.getByRole("button", { name: "Continue with Discord" }).click();
            await page.getByRole("heading", { name: "Pick a nickname" }).waitFor();
            await settle(page);
            await shot();
        },
    },
    {
        name: "inbox", as: "heroes-of-the-storm@example.com", run: async (page, shot, device) => {
            await open(page, "/messages");
            await shot();
            await page.locator(".inbox-row").first().click();
            await page.locator("#spa-teamtavern > [data-path^='/messages/']").waitFor();
            await settle(page);
            await shot("thread");
        },
    },
    {
        name: "block", signUp: "B", run: async (page, shot) => {
            const panel = await openPanel(page, "dota-2", "Dota2Tester");
            await panel.getByRole("button", { name: "More" }).click();
            await shot("menu");
            await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Block Dota2Tester" }).click();
            await shot("confirm");
            await panel.getByRole("button", { name: "Block Dota2Tester" }).click();
            await page.locator(".toast").waitFor();
            await shot("toast");
            await page.locator(".toast").getByRole("button", { name: "Undo" }).click();
        },
    },
    {
        name: "report", signUp: "R", run: async (page, shot) => {
            const panel = await openPanel(page, "dota-2", "Dota2Tester");
            await panel.getByRole("button", { name: "More" }).click();
            await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Report this post" }).click();
            await shot();
        },
    },
    {
        name: "account", as: "owner@example.com", run: async (page, shot) => {
            await open(page, "/account");
            await shot();
            await page.getByRole("button", { name: "Edit" }).click();
            await shot("edit");
            await page.getByRole("button", { name: "Cancel" }).click();
            await page.getByRole("button", { name: "Delete account" }).click();
            await shot("delete");
        },
    },
];

// HeroesOfTheStormTester's inbox holds a conversation only once somebody writes to them.
async function writeToHeroesOfTheStormTester(browser) {
    const context = await browser.newContext({ baseURL: origin });
    const page = await context.newPage();
    await signUp(page, "W");
    const panel = await openPanel(page, "heroes-of-the-storm", "HeroesOfTheStormTester");
    await panel.getByRole("textbox", { name: "Message" }).fill("Hi, want to play tonight?\nI'm on from nine.");
    await panel.getByRole("button", { name: "Send" }).click();
    await panel.locator(".message-own").waitFor();
    await context.close();
}

const chosen = scenes.filter(scene => only.length === 0 || only.some(text => scene.name.includes(text)));
mkdirSync(join(here, "screenshots"), { recursive: true });
const browser = await chromium.launch();
if (chosen.some(scene => scene.name === "inbox")) {
    await writeToHeroesOfTheStormTester(browser);
}
let failed = false;
for (const scene of chosen) {
    for (const [device, width] of Object.entries(widths)) {
        if (scene.devices && !scene.devices.includes(device)) {
            continue;
        }
        const context = await browser.newContext({ baseURL: origin, viewport: { width, height: 800 } });
        // The ad script fills nothing on a local origin and only adds errors of its own.
        await context.route(url => url.origin !== origin && url.hostname !== "discord.com", route => route.abort());
        const page = await context.newPage();
        page.on("pageerror", error => console.error(`${scene.name} ${device}: ${error.message}`));
        // A refused request, such as /api/me signed out, is logged by the browser itself.
        page.on("console", message => message.type() === "error" && !message.text().startsWith("Failed to load resource")
            && console.error(`${scene.name} ${device}: ${message.text()}`));
        // A full-page shot stretches the viewport to the page, which moves whatever is
        // fixed to it, so a page with an overlay open is shot as the player sees it.
        const shot = async part => {
            const file = join(here, "screenshots", `${scene.name}${part ? `-${part}` : ""}-${device}.png`);
            const overlaid = await page.locator(".overlay-layer").count() > 0;
            // The sticky header is drawn where the window was scrolled to.
            if (!overlaid) {
                await page.evaluate(() => window.scrollTo(0, 0));
            }
            await page.screenshot({ path: file, fullPage: !overlaid });
            console.log(file);
        };
        try {
            if (scene.as) {
                await signIn(page, scene.as);
            } else if (scene.signUp) {
                await signUp(page, scene.signUp);
            }
            await scene.run(page, shot, device);
        } catch (error) {
            failed = true;
            console.error(`${scene.name} ${device} failed: ${error.message}`);
        }
        await context.close();
    }
}
await browser.close();
process.exit(failed ? 1 : 0);
