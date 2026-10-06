import { expect, Page } from "@playwright/test";
import { unique } from "./accounts";
import { expectPage } from "./pages";

// An individual account's SteamID64, random, since specs in other workers make theirs too.
export function steamId(): string {
    return "7656119" + String(Math.floor(Math.random() * 1e10)).padStart(10, "0");
}

// The nickname the steam service's GetPlayerSummaries gives an account's profile name,
// `s ` and the last eight digits of its ID, once the space a nickname can't hold is gone
// (see `SteamStub/Main.purs`).
export const steamNickname = (id: string) => `s${id.slice(9)}`;

// The pages send the browser to Steam's OpenID sign-in, and Steam sends it back to the page
// named in `openid.return_to` with its answer in the query. This plays Steam's part: it
// answers for whichever account is set at the time, or turns back as a player does who
// cancels at Steam, and records each sign-in URL and each answer, so a test can check what
// the page asked for and send an answer again. The steam service in the test stack checks
// the answers, passing each once, as Steam does.
export type FakeSteam = { steamId: string, cancel: boolean, loginRequests: URL[], answers: string[] };

// The address Steam sends the browser to when it vouches for the account: `answerTo` with
// the `openid.` parameters that say so after it, the answer given at `givenAt`. The
// signature is the steam service's to check, which takes any.
export function steamAnswer(answerTo: string, id: string, returnTo = answerTo, givenAt = new Date()): string {
    const claimedId = `https://steamcommunity.com/openid/id/${id}`;
    return withParams(answerTo, {
        "openid.ns": "http://specs.openid.net/auth/2.0",
        "openid.mode": "id_res",
        "openid.op_endpoint": "https://steamcommunity.com/openid/login",
        "openid.claimed_id": claimedId,
        "openid.identity": claimedId,
        "openid.return_to": returnTo,
        "openid.response_nonce": `${givenAt.toISOString().slice(0, 19)}Z${unique("")}`,
        "openid.assoc_handle": "1234567890",
        "openid.signed": "signed,op_endpoint,claimed_id,identity,return_to,response_nonce,assoc_handle",
        "openid.sig": "c3R1Yi1zaWduYXR1cmU+Lw==",
    });
}

function withParams(address: string, params: Record<string, string>): string {
    const url = new URL(address);
    for (const [key, value] of Object.entries(params)) url.searchParams.append(key, value);
    return url.href;
}

export async function fakeSteam(page: Page, id = steamId()): Promise<FakeSteam> {
    const steam: FakeSteam = { steamId: id, cancel: false, loginRequests: [], answers: [] };
    await page.route(url => url.hostname === "steamcommunity.com" && url.pathname === "/openid/login", route => {
        const login = new URL(route.request().url());
        steam.loginRequests.push(login);
        const returnTo = login.searchParams.get("openid.return_to")!;
        const answer = steam.cancel
            ? withParams(returnTo, { "openid.ns": "http://specs.openid.net/auth/2.0", "openid.mode": "cancel" })
            : steamAnswer(returnTo, steam.steamId);
        steam.answers.push(answer);
        return route.fulfill({ status: 302, headers: { location: answer } });
    });
    return steam;
}

// Starts a trip to Steam in the tab as a page would, without going there, so a test can
// hand the sign-in page an answer of its own for it. The tab has to be on the site.
export async function startSteamTrip(page: Page, state: string, back = "/") {
    await page.evaluate(
        trip => sessionStorage.setItem("tt-steam", JSON.stringify(trip)),
        { state, back, from: "/signin", switching: false });
}

// A Steam player new to the site picks a nickname once Steam sends them back, which their
// Steam profile name prefills, and gives an email, which Steam doesn't share.
export async function signUpWithSteam(page: Page, steam: FakeSteam, nickname = unique("S")): Promise<string> {
    await page.goto("/signup");
    await page.getByRole("button", { name: "Continue with Steam" }).click();
    await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
    await expect(page.getByLabel("Nickname")).toHaveValue(steamNickname(steam.steamId));
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByRole("button", { name: "Continue" }).click();
    await expectPage(page, "/");
    return nickname;
}
