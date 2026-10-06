import { expect, Page, test } from "@playwright/test";
import { signIn, signUp } from "../accounts";
import { expectPage } from "../pages";

// Posts go into Deadlock, whose feed no other spec reads, and whose trackers all take the
// SteamID64 the account keeps. The test stack's Steam (`SteamStub/Main.purs`) knows one
// custom profile address, Gabe Newell's `gabelogannewell`, whose profile is 76561197960287930.
const steamId = "76561197960287930";

const trackers = [
    `https://statlocker.gg/profile/${steamId}`,
    `https://tracker.gg/deadlock/profile/steam/${steamId}`,
    `https://mobalytics.gg/deadlock/player-profile/${steamId}`,
    `https://deadlock-tracker.com/players/${steamId}`,
];

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openPostScreen(page: Page) {
    await page.goto("/games/deadlock/post/player");
    await expect(page.getByRole("heading", { name: "Tell groups and players about you" })).toBeVisible();
    await expect(page.getByText("Paste your Steam profile's link, or your 17-digit SteamID.")).toBeVisible();
}

async function publish(page: Page) {
    await page.getByRole("button", { name: "Publish post" }).click();
    await expectPage(page, "/games/deadlock/post/player/live");
}

// The tracker links on the player's own post page, where the card shows them unfolded.
async function expectTrackers(page: Page, nickname: string) {
    await page.goto("/");
    await page.locator(".own-post").getByRole("link", { name: nickname, exact: true }).click();
    await expectPage(page, /^\/games\/deadlock\/posts\/\d+$/);
    await expect(page.locator(".card .detail a")).toHaveCount(trackers.length);
    expect(await page.locator(".card .detail a").evaluateAll(links => links.map(link => link.getAttribute("href"))))
        .toEqual(trackers);
}

test.describe("a Steam contact", () => {
    test("takes a profile's link, and the card links the profile and trackers by its SteamID", async ({ page, browser }) => {
        const nickname = await signUp(page, "S");
        await openPostScreen(page);
        await page.getByLabel("Steam profile").fill(`https://steamcommunity.com/profiles/${steamId}/`);
        await publish(page);
        await expectTrackers(page, nickname);

        const visitor = await (await browser.newContext()).newPage();
        await signIn(visitor, "new@example.com");
        await visitor.goto("/games/deadlock");
        await expect(visitor.locator(".feed")).toHaveAttribute("aria-busy", "false");
        await card(visitor, nickname).locator(".card-contact").click();
        const steam = visitor.getByRole("dialog", { name: nickname }).locator(".contact-row")
            .filter({ hasText: "Steam profile" }).locator("a.contact-value");
        await expect(steam).toHaveText(steamId);
        await expect(steam).toHaveAttribute("href", `https://steamcommunity.com/profiles/${steamId}`);
    });

    test("takes a custom profile address, asking Steam for its SteamID", async ({ page }) => {
        const nickname = await signUp(page, "S");
        await openPostScreen(page);
        await page.getByLabel("Steam profile").fill("steamcommunity.com/id/gabelogannewell");
        await publish(page);
        await expectTrackers(page, nickname);

        await page.goto("/account");
        await expect(page.getByRole("heading", { name: "Shown on your posts" })).toBeVisible();
        const contacts = page.locator(".data-row").filter({ hasText: "Contacts" }).locator(".data-value");
        await expect(contacts).toHaveText(`Steam profile ${steamId}`);

        await page.getByRole("button", { name: "Edit" }).click();
        await page.getByLabel("Steam profile").fill("steamcommunity.com/id/no-such-profile");
        await page.getByRole("button", { name: "Save changes" }).click();
        await expect(page.getByText("That isn't a Steam profile. Paste your profile's link, or your SteamID.")).toBeVisible();
        await page.getByRole("button", { name: "Cancel" }).click();
        await expect(contacts).toHaveText(`Steam profile ${steamId}`);
    });

    test("turns away a name, or a custom address no profile has", async ({ page }) => {
        await signUp(page, "S");
        await openPostScreen(page);
        const error = page.getByText("That isn't a Steam profile. Paste your profile's link, or your SteamID.");

        await page.getByLabel("Steam profile").fill("xXSniperXx");
        await page.getByRole("button", { name: "Publish post" }).click();
        await expect(error).toBeVisible();
        await expect(page.getByLabel("Steam profile")).toBeFocused();
        await expectPage(page, "/games/deadlock/post/player");

        await page.getByLabel("Steam profile").fill("https://steamcommunity.com/id/no-such-profile/");
        await page.getByRole("button", { name: "Publish post" }).click();
        await expect(error).toBeVisible();
        await expectPage(page, "/games/deadlock/post/player");
    });

    // The stub answers 503 for this one name.
    test("says when Steam doesn't answer, and takes the SteamID instead", async ({ page }) => {
        const nickname = await signUp(page, "S");
        await openPostScreen(page);
        await page.getByLabel("Steam profile").fill("steamcommunity.com/id/steam-is-down");

        await page.getByRole("button", { name: "Publish post" }).click();

        await expect(page.getByText("Steam didn't answer. Try again, or paste your SteamID instead.")).toBeVisible();
        await expect(page.getByLabel("Steam profile")).toBeFocused();
        await expectPage(page, "/games/deadlock/post/player");

        await page.getByLabel("Steam profile").fill(steamId);
        await publish(page);
        await expectTrackers(page, nickname);
    });
});
