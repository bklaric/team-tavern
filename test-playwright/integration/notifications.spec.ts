import { expect, Page, test } from "@playwright/test";
import { signIn, signUp } from "../accounts";
import { expectPage } from "../pages";

// Posts go into Valheim, whose feed no other spec asserts. ValheimTester's player post
// (`stacks/test-seed/players.sql`) answers Vanilla, PC, Casual and New characters, and like
// every tester's account says Croatia, English, a birthday in 2000, a microphone and
// 19:00–23:00 in Zagreb. A post that says the same fits it, and it fits that post back, so
// the post's Matches and ValheimTester's bell show the same pair. OwnerTester's Valheim post
// answers no fields and shares the facts, so the post fits it too.

// The field whose label reads the text, for its controls.
const field = (page: Page, label: string) =>
    page.locator(".field").filter({ has: page.locator(".field-label", { hasText: new RegExp(`^${label}$`) }) });

// A pill is its label: the box inside it is hidden, as a player never sees it.
const pill = (page: Page, group: string, text: string) =>
    page.getByRole("group", { name: group }).locator(".pill", { hasText: new RegExp(`^${text}$`) });

const bell = (page: Page) => page.getByRole("button", { name: /^Notifications/ });

// The count the bell's label reads out, "Notifications, 2 new".
async function unread(page: Page): Promise<number> {
    const label = await bell(page).getAttribute("aria-label");
    const count = label?.match(/, (\d+) new$/);
    return count ? Number(count[1]) : 0;
}

const bellSays = (count: number) => (count ? `Notifications, ${count} new` : "Notifications");

// A new player publishing a Valheim player post that fits ValheimTester's, landing on
// Matches with ValheimTester among what fits.
async function publishFitting(page: Page): Promise<string> {
    const nickname = await signUp(page, "N");
    await page.goto("/games/valheim/post/player");
    await expect(page.getByRole("heading", { name: "Tell groups and players about you" })).toBeVisible();
    await pill(page, "Server type", "Vanilla").click();
    await pill(page, "Platform", "PC").click();
    await pill(page, "Looking for", "Casual").click();
    await pill(page, "Server characters", "New characters").click();
    await page.getByLabel("Location").selectOption("Croatia");
    await page.getByLabel("Languages").selectOption("English");
    await page.getByLabel("Birthday").fill("2000-01-10");
    await page.getByLabel("I use a microphone").check();
    await field(page, "Usually online").getByLabel("From").selectOption("20:00");
    await field(page, "Usually online").getByLabel("To").selectOption("23:00");
    await page.getByLabel("Timezone").selectOption("Europe/Zagreb");
    await page.getByRole("button", { name: "Publish post" }).click();

    await expectPage(page, "/games/valheim/post/player/live");
    await expect(page.getByRole("heading", { name: /fits? you right now$/ })).toBeVisible();
    await expect(page.locator(".card").filter({ has: page.getByRole("link", { name: "ValheimTester", exact: true }) }))
        .toHaveCount(1);
    return nickname;
}

test.describe("notifications", () => {
    test("tell the owner of a post that a new one fits it, and open it", async ({ browser }) => {
        const publisher = await (await browser.newContext()).newPage();
        const owner = await (await browser.newContext()).newPage();
        const nickname = await publishFitting(publisher);

        await signIn(owner, "valheim@example.com");
        await expect(bell(owner)).toHaveAccessibleName(/^Notifications, \d+ new$/);
        const before = await unread(owner);
        await bell(owner).click();
        const list = owner.getByRole("dialog", { name: "Notifications" });
        const group = list.locator(".notification-group")
            .filter({ has: owner.getByRole("heading", { name: "ValheimTester · Valheim player" }) });
        const row = group.locator(".notification", { hasText: `${nickname} fits` });
        await expect(row).toHaveClass(/notification-unread/);
        await expect(row.locator(".notification-meta")).toHaveText("Player · just now");

        // Opening it reads it, which the bell counts once the page has changed.
        await row.click();
        await expectPage(owner, /^\/games\/valheim\/posts\/\d+$/);
        await expect(owner.getByRole("heading", { level: 1, name: nickname })).toBeVisible();
        await expect(bell(owner)).toHaveAccessibleName(bellSays(before - 1));
        await bell(owner).click();
        await expect(row).not.toHaveClass(/notification-unread/);
    });

    // OwnerTester's post in its last week holds its seeded expiry, unread, so with the new
    // post there are two unread rows under it, and Mark all read has something to do.
    test("put a post's own expiry before what fits it, and read them all", async ({ browser }) => {
        const publisher = await (await browser.newContext()).newPage();
        const owner = await (await browser.newContext()).newPage();
        const nickname = await publishFitting(publisher);

        await signIn(owner, "owner@example.com");
        await owner.goto("/games/valheim");
        await expectPage(owner, "/games/valheim");
        await bell(owner).click();
        const list = owner.getByRole("dialog", { name: "Notifications" });
        const group = list.locator(".notification-group")
            .filter({ has: owner.getByRole("heading", { name: "OwnerTester · Valheim player" }) });
        const rows = group.locator(".notification");
        await expect(rows.first().locator(".notification-title")).toHaveText(/^Expires in \d+ days?$/);
        await expect(rows.first().locator(".notification-meta")).toHaveText("Renew it from your posts.");
        await expect(rows.nth(1).locator(".notification-title")).toHaveText(`${nickname} fits`);
        await expect(rows.nth(1)).toHaveClass(/notification-unread/);

        await list.getByRole("button", { name: "Mark all read" }).click();
        await expect(list.locator(".notification-unread")).toHaveCount(0);
        await expect(list.getByRole("button", { name: "Mark all read" })).toHaveCount(0);
        await expect(list.locator(".notification").first()).toBeFocused();
        await expect(bell(owner)).toHaveAccessibleName("Notifications");

        // A post of the player's about to expire opens their posts, where it is renewed.
        await rows.first().click();
        await expectPage(owner, "/");
        await expect(owner.getByRole("heading", { name: "Your posts", level: 1 })).toBeVisible();
    });
});
