import AxeBuilder from "@axe-core/playwright";
import { Browser, expect, Page, test } from "@playwright/test";
import { signIn, signUp } from "../accounts";
import { expectPage } from "../pages";

// axe checks every page kind and every overlay against WCAG 2.1 A and AA, on a desktop
// and on a phone. The ad network's script is kept out: it fills nothing on a local origin.

const settled = (page: Page) => expect(page.locator("[aria-busy=true]")).toHaveCount(0);

async function expectAccessible(page: Page, scene: string) {
    const results = await new AxeBuilder({ page })
        .withTags(["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"])
        .exclude(".ad")
        .analyze();
    const violations = results.violations.map(violation =>
        `${violation.id}: ${violation.help} (${violation.nodes.map(node => node.target.join(" ")).join(", ")})`);
    expect.soft(violations, scene).toEqual([]);
}

async function visit(page: Page, path: string) {
    await page.goto(path);
    await expectPage(page, path.split(/[?#]/)[0]);
    await settled(page);
    await expectAccessible(page, path);
}

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openFeed(page: Page, handle: string) {
    await page.goto(`/games/${handle}`);
    await settled(page);
}

test.beforeEach(async ({ context }) => {
    await context.route(url => url.hostname === "hb.vntsm.com", route => route.abort());
});

for (const [device, viewport] of [["desktop", { width: 1280, height: 800 }], ["phone", { width: 375, height: 740 }]] as const) {
    test.describe(`on a ${device}`, () => {
        test.use({ viewport });

        test("pages and overlays signed out pass axe", async ({ page }) => {
            for (const path of ["/", "/games/valorant", "/games/valorant/posts/999999", "/signin", "/signup",
                "/forgot-password", "/privacy", "/post", "/post/player", "/games/valorant/post/player",
                "/messages", "/no-such-page"]) {
                await visit(page, path);
            }

            await openFeed(page, "valorant");
            await card(page, "Night Owls").getByRole("link", { name: "Night Owls", exact: true }).click();
            await expectPage(page, /^\/games\/valorant\/posts\/\d+$/);
            await settled(page);
            await expectAccessible(page, "post page");

            await openFeed(page, "valorant");
            await card(page, "Night Owls").getByRole("button", { name: "Details" }).click();
            await expectAccessible(page, "card details");

            if (device === "phone") {
                await page.locator(".description-summary").click();
                await expect(page.getByRole("dialog", { name: "Tell us about you" })).toBeVisible();
                await expectAccessible(page, "description sheet");
                await page.keyboard.press("Escape");
                await page.getByRole("button", { name: "Menu" }).click();
                await expect(page.getByRole("dialog", { name: "Menu" })).toBeVisible();
                await expectAccessible(page, "signed-out menu");
                await page.keyboard.press("Escape");
            } else {
                await page.getByRole("button", { name: "Rank", exact: true }).click();
                await expect(page.getByRole("dialog", { name: "Rank" })).toBeVisible();
                await expectAccessible(page, "rank popover");
                await page.keyboard.press("Escape");
            }

            await page.getByRole("button", { name: "Games" }).click();
            await expect(page.getByRole("dialog", { name: "Games" })).toBeVisible();
            await expectAccessible(page, "games");
            await page.keyboard.press("Escape");

            await visit(page, "/signup");
            await page.getByRole("button", { name: "Create account" }).click();
            await expect(page.getByRole("alert").first()).toBeVisible();
            await expectAccessible(page, "sign-up errors");
            // A field's error describes its control, which says it is invalid.
            const invalid = page.locator("[aria-invalid=true]").first();
            await expect(invalid).toBeVisible();
            const errorId = await invalid.getAttribute("aria-describedby");
            await expect(page.locator(`[id="${errorId}"]`)).toHaveRole("alert");
        });

        test("pages and overlays signed in pass axe", async ({ page }) => {
            await signIn(page, "owner@example.com");
            for (const path of ["/", "/account", "/messages", "/games/valorant/post/group"]) {
                await visit(page, path);
            }
            await page.getByRole("button", { name: /^Notifications/ }).click();
            await expect(page.getByRole("dialog", { name: "Notifications" }).locator(".notification").first()).toBeVisible();
            await expectAccessible(page, "notifications");
            await page.keyboard.press("Escape");
            await page.getByRole("button", { name: "Account menu" }).click();
            await expect(page.getByRole(device === "phone" ? "dialog" : "group", { name: "OwnerTester" })).toBeVisible();
            await expectAccessible(page, "account menu");
            await page.keyboard.press("Escape");

            if (device === "phone") {
                await page.getByRole("button", { name: "Preview" }).click();
                await expect(page.getByRole("dialog", { name: "Preview" })).toBeVisible();
                await expectAccessible(page, "preview sheet");
                await page.keyboard.press("Escape");
            }

            await visit(page, "/account");
            await page.getByRole("button", { name: "Edit" }).click();
            await expectAccessible(page, "account edit");
            await page.getByRole("button", { name: "Cancel" }).click();
            await page.getByRole("button", { name: "Delete account" }).click();
            await expectAccessible(page, "account delete");

            await page.goto("/");
            await settled(page);
            await page.getByRole("link", { name: "Kestrel's Nest", exact: true }).click();
            await expectPage(page, /^\/games\/dota-2\/posts\/\d+$/);
            await settled(page);
            await expectAccessible(page, "owner's post page");
        });

        test("posting, the panel and the inbox pass axe", async ({ browser }) => {
            test.setTimeout(90_000);
            const { page: writer, nickname } = await newPlayer(browser, viewport);
            await visit(writer, "/games/valorant/post/group");
            await openFeed(writer, "heroes-of-the-storm");
            await card(writer, "HeroesOfTheStormTester").locator(".card-contact").click();
            const panel = writer.getByRole("dialog", { name: "HeroesOfTheStormTester" });
            await expect(panel.getByRole("textbox", { name: "Message" })).toBeVisible();
            await settled(writer);
            await expectAccessible(writer, "contact panel");
            await panel.getByRole("textbox", { name: "Message" }).fill("Want to play tonight?");
            await panel.getByRole("button", { name: "Send" }).click();
            await expect(panel.locator(".message-own")).toHaveCount(1);
            await expectAccessible(writer, "contact panel with a thread");

            await panel.getByRole("button", { name: "More" }).click();
            await expectAccessible(writer, "more menu");
            await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Block HeroesOfTheStormTester" }).click();
            await expectAccessible(writer, "block confirmation");
            await panel.getByRole("button", { name: "Cancel" }).click();
            await panel.getByRole("button", { name: "More" }).click();
            await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Report this post" }).click();
            await panel.getByRole("button", { name: "Send report" }).click();
            await expect(panel.getByRole("alert")).toBeVisible();
            await expectAccessible(writer, "report form with its error");
            await writer.keyboard.press("Escape");
            await writer.keyboard.press("Escape");

            await visit(writer, "/messages");
            await writer.locator(".inbox-row").first().click();
            await expectPage(writer, /^\/messages\/\d+$/);
            await settled(writer);
            await expectAccessible(writer, `conversation of ${nickname}`);
        });
    });
}

// What axe can't see: where the focus goes, and what the keyboard reaches.
test.describe("with the keyboard", () => {
    test("the skip link and a link to another page put the focus on the page's content", async ({ page }) => {
        await page.goto("/");
        await settled(page);
        await page.keyboard.press("Tab");
        const skip = page.getByRole("link", { name: "Skip to content" });
        await expect(skip).toBeFocused();
        await page.keyboard.press("Enter");
        await expect(page.locator("main#content")).toBeFocused();

        await page.getByRole("link", { name: "Valorant" }).first().click();
        await expectPage(page, "/games/valorant");
        await expect(page.locator("main#content")).toBeFocused();
    });

    test("Tab goes round the contact panel, whose page stays out of reach, and its toast stays live", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "valorant");
        await card(page, "ValorantTester").locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: "ValorantTester" });
        await expect(panel.getByRole("textbox", { name: "Message" })).toBeFocused();
        await expect(page.locator(".site-header-root")).toHaveJSProperty("inert", true);

        const more = panel.getByRole("button", { name: "More" });
        const lastCopy = panel.getByRole("button", { name: /^Copy/ }).last();
        await lastCopy.focus();
        await page.keyboard.press("Tab");
        await expect(more).toBeFocused();
        await page.keyboard.press("Shift+Tab");
        await expect(lastCopy).toBeFocused();

        await more.click();
        await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Report this post" }).click();
        await panel.getByRole("radio", { name: "Spam or advertising" }).check();
        await panel.getByRole("button", { name: "Send report" }).click();
        await expect(page.locator(".toast-text")).toHaveText("Report sent. Thanks for telling us.");
        await expect(panel).toBeVisible();
        expect(await page.locator(".toasts").evaluate(toasts => toasts.closest("[inert]") === null)).toBe(true);
    });

    test("a dropdown closes once the focus leaves it, and the list the bell opens takes the focus as it comes",
        async ({ page }) => {
        await signIn(page, "owner@example.com");
        await page.getByRole("button", { name: "Account menu" }).click();
        const menu = page.getByRole("group", { name: "OwnerTester" });
        await menu.getByRole("button", { name: "Sign out" }).focus();
        await page.keyboard.press("Tab");
        await expect(menu).toHaveCount(0);

        // Whether other specs have read them or not, the list has something to focus.
        await page.getByRole("button", { name: /^Notifications/ }).click();
        await expect(page.getByRole("dialog", { name: "Notifications" }).locator(":focus")).toHaveCount(1);
    });

    test("Escape cancels a confirmation and gives the focus back to what asked for it", async ({ page }) => {
        await signIn(page, "group@example.com");
        await page.goto("/games/valorant/post/group");
        await settled(page);
        await page.getByRole("button", { name: "Delete it" }).click();
        await expect(page.getByRole("button", { name: "Keep it" })).toBeFocused();
        await page.keyboard.press("Escape");
        await expect(page.getByRole("alertdialog")).toHaveCount(0);
        await expect(page.getByRole("button", { name: "Delete it" })).toBeFocused();

        await visit(page, "/account");
        await page.getByRole("button", { name: "Delete account" }).click();
        await expect(page.getByRole("button", { name: "Keep it" })).toBeFocused();
        await page.keyboard.press("Escape");
        await expect(page.getByRole("alertdialog")).toHaveCount(0);
        await expect(page.getByRole("button", { name: "Delete account" })).toBeFocused();

        await openFeed(page, "valorant");
        await card(page, "ValorantTester").locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: "ValorantTester" });
        await panel.getByRole("button", { name: "More" }).click();
        await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Block ValorantTester" }).click();
        await expect(panel.getByRole("button", { name: "Cancel" })).toBeFocused();
        await page.keyboard.press("Escape");
        await expect(panel.getByRole("alertdialog")).toHaveCount(0);
        await expect(panel).toBeVisible();
    });

    test("a stepper at its end keeps the focus", async ({ page }) => {
        await signIn(page, "new@example.com");
        await page.goto("/games/valorant/post/group");
        await settled(page);
        const fewer = page.getByRole("group", { name: "Players in the group" }).getByRole("button", { name: "Fewer" });
        await fewer.focus();
        for (let i = 0; i < 6; i++) {
            await page.keyboard.press("Enter");
        }
        await expect(fewer).toHaveAttribute("aria-disabled", "true");
        await expect(fewer).toBeFocused();
    });

    test.describe("on a phone", () => {
        test.use({ viewport: { width: 375, height: 740 } });

        test("the description sheet opens on the type chosen", async ({ page }) => {
            await openFeed(page, "valorant");
            await page.locator(".description-summary").click();
            const sheet = page.getByRole("dialog", { name: "Tell us about you" });
            await expect(sheet.getByRole("radio", { name: "I'm a player looking for a group" })).toBeFocused();
            await sheet.getByRole("radio", { name: "We're a group looking for players" }).check();
            await page.keyboard.press("Escape");
            await settled(page);

            await page.locator(".description-summary").click();
            await expect(sheet.getByRole("radio", { name: "We're a group looking for players" })).toBeFocused();
        });
    });
});

async function newPlayer(browser: Browser, viewport: { width: number, height: number }) {
    const context = await browser.newContext({ viewport });
    await context.route(url => url.hostname === "hb.vntsm.com", route => route.abort());
    const page = await context.newPage();
    const nickname = await signUp(page, "A");
    return { page, nickname };
}
