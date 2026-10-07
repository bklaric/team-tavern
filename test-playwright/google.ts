import { expect, Page } from "@playwright/test";
import { unique } from "./accounts";
import { expectPage } from "./pages";

export type GoogleUser = { sub: string, email: string | null, verified: boolean, name: string | null };

export function googleUser(email: string | null, verified: boolean, name: string | null = `G ${unique("")}`): GoogleUser {
    return { sub: unique(""), email, verified, name };
}

// The nickname the sign-in page offers for a Google name: its letters, digits, dashes,
// underscores and dots, as many as a nickname holds.
export const googleNickname = (name: string) => name.replace(/[^A-Za-z0-9_.-]/g, "").slice(0, 40);

// The pages send the browser to Google's authorize URL, and Google sends it back to the
// page named in `redirect_uri` with a code and the page's `state` in the query. This plays
// Google's part: it answers with a code for whichever user is set at the time and the
// redirect URI the page named, which the google service in the test stack swaps for that
// user's claims only with that same URI (see `GoogleStub/Main.purs`), or turns back as a
// player does who cancels at Google, and records each authorize URL and each code, so a
// test can check what the page asked for and send a code again. Each code is new, since
// the google service swaps each once, as Google does, unless `replay` sends the last one
// again.
export type FakeGoogle = { user: GoogleUser, cancel: boolean, replay: boolean, authorizeRequests: URL[], codes: string[] };

export async function fakeGoogle(page: Page, user: GoogleUser): Promise<FakeGoogle> {
    const google: FakeGoogle = { user, cancel: false, replay: false, authorizeRequests: [], codes: [] };
    await page.route(url => url.hostname === "accounts.google.com" && url.pathname === "/o/oauth2/v2/auth", route => {
        const authorize = new URL(route.request().url());
        google.authorizeRequests.push(authorize);
        const { sub, email, verified, name } = google.user;
        const redirect_uri = authorize.searchParams.get("redirect_uri");
        const code = google.replay && google.codes.length > 0
            ? google.codes[google.codes.length - 1]
            : JSON.stringify({ sub, email, email_verified: verified, name, redirect_uri, code: unique("") });
        if (!google.cancel) google.codes.push(code);
        const state = encodeURIComponent(authorize.searchParams.get("state") ?? "");
        const answer = google.cancel
            ? `error=access_denied&state=${state}`
            : `state=${state}&code=${encodeURIComponent(code)}&scope=email+profile+openid&authuser=0&prompt=consent`;
        return route.fulfill({
            status: 302,
            headers: { location: `${redirect_uri}?${answer}` },
        });
    });
    return google;
}

// A Google player new to the site picks a nickname once Google sends them back, which their
// Google name prefills.
export async function signUpWithGoogle(page: Page, google: FakeGoogle, nickname = unique("G")): Promise<string> {
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Google" }).click();
    await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
    await expect(page.getByLabel("Nickname")).toHaveValue(googleNickname(google.user.name ?? ""));
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    return nickname;
}
