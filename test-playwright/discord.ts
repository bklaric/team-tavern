import { expect, Page } from "@playwright/test";
import { unique } from "./accounts";
import { expectPage } from "./pages";

export type DiscordUser = { id: string, username: string, email: string | null, verified: boolean };

export function discordUser(email: string | null, verified: boolean): DiscordUser {
    const id = unique("");
    return { id, username: `d${id}`, email, verified };
}

// The pages send the browser to Discord's authorize URL, and Discord sends it back to the
// page named in `redirect_uri` with a code and the page's `state` in the query. This plays
// Discord's part: it answers with a code for whichever user is set at the time, the app the
// page named, unless `application` names another, and the redirect URI the page named,
// which the discord service in the local stack swaps for that user only for that same app
// and URI (see `DiscordStub/Main.purs`), or turns back as a player does who cancels at
// Discord, and records each authorize URL and each code, so a test can check what the page
// asked for and send a code again. Each code is new, since the discord service swaps each
// once, as Discord does, unless `replay` sends the last one again.
export type FakeDiscord = {
    user: DiscordUser,
    cancel: boolean,
    replay: boolean,
    application: string | null,
    authorizeRequests: URL[],
    codes: string[],
};

export async function fakeDiscord(page: Page, user: DiscordUser): Promise<FakeDiscord> {
    const discord: FakeDiscord = { user, cancel: false, replay: false, application: null, authorizeRequests: [], codes: [] };
    await page.route(url => url.hostname === "discord.com" && url.pathname === "/api/oauth2/authorize", route => {
        const authorize = new URL(route.request().url());
        discord.authorizeRequests.push(authorize);
        const { id, username, email, verified } = discord.user;
        const client_id = discord.application ?? authorize.searchParams.get("client_id");
        const redirect_uri = authorize.searchParams.get("redirect_uri");
        const code = discord.replay && discord.codes.length > 0
            ? discord.codes[discord.codes.length - 1]
            : JSON.stringify({ id, username, discriminator: "0", email, verified, client_id, redirect_uri, code: unique("") });
        if (!discord.cancel) discord.codes.push(code);
        const state = encodeURIComponent(authorize.searchParams.get("state") ?? "");
        const answer = discord.cancel
            ? `error=access_denied&error_description=The+resource+owner+or+authorization+server+denied+the+request&state=${state}`
            : `code=${encodeURIComponent(code)}&state=${state}`;
        return route.fulfill({
            status: 302,
            headers: { location: `${redirect_uri}?${answer}` },
        });
    });
    return discord;
}

// A Discord player new to the site picks a nickname once Discord sends them back, which
// Discord's username prefills.
export async function signUpWithDiscord(page: Page, discord: FakeDiscord, nickname = unique("D")): Promise<string> {
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Discord" }).click();
    await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
    await expect(page.getByLabel("Nickname")).toHaveValue(discord.user.username);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    return nickname;
}
