import { expect, Page } from "@playwright/test";
import { unique } from "./accounts";
import { expectPage } from "./pages";

export type DiscordUser = { id: string, username: string, email: string | null, verified: boolean };

export function discordUser(email: string | null, verified: boolean): DiscordUser {
    const id = unique("");
    return { id, username: `d${id}`, email, verified };
}

// The pages send the browser to Discord's authorize URL, and Discord sends it back to the
// page named in `redirect_uri` with an access token and the page's `state` in the fragment.
// This plays Discord's part: it answers with a token for whichever user is set at the time,
// which the discord service in the test stack reads back as that user (see
// `DiscordStub/Main.purs`), and records each authorize URL so a test can check what the
// page asked for.
export type FakeDiscord = { user: DiscordUser, authorizeRequests: URL[] };

export async function fakeDiscord(page: Page, user: DiscordUser): Promise<FakeDiscord> {
    const discord: FakeDiscord = { user, authorizeRequests: [] };
    await page.route(url => url.hostname === "discord.com" && url.pathname === "/api/oauth2/authorize", route => {
        const authorize = new URL(route.request().url());
        discord.authorizeRequests.push(authorize);
        const { id, username, email, verified } = discord.user;
        const token = encodeURIComponent(JSON.stringify({ id, username, discriminator: "0", email, verified }));
        const state = encodeURIComponent(authorize.searchParams.get("state") ?? "");
        return route.fulfill({
            status: 302,
            headers: {
                location: `${authorize.searchParams.get("redirect_uri")}#token_type=Bearer&access_token=${token}&state=${state}`,
            },
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
