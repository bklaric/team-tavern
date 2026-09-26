import { expect, test } from "@playwright/test";
import { signIn } from "../accounts";
import { expectPage } from "../pages";

// A feed is headed by its game's title.
const feedHeading = { name: "Apex Legends", level: 1 };

test("a link clicked with Ctrl or Cmd opens in a new tab and leaves this one", async ({ page }) => {
    await page.goto("/games/apex-legends");

    const [tab] = await Promise.all([
        page.context().waitForEvent("page"),
        page.getByRole("banner").getByRole("link", { name: "Sign in" }).click({ modifiers: ["ControlOrMeta"] }),
    ]);

    await expect(tab).toHaveURL(url => url.pathname === "/signin");
    await expectPage(page, "/games/apex-legends");
    await expect(page.getByRole("heading", feedHeading)).toBeVisible();
});

for (const [link, path, heading] of [
    ["About", "/about", "About TeamTavern"],
    ["Contact", "/contact", "Contact"],
    ["Terms", "/terms", "Terms of use"],
    ["Privacy", "/privacy", "Privacy policy"],
])
    test(`the footer leads to ${path}`, async ({ page }) => {
        await page.goto("/games/apex-legends");

        await page.getByRole("contentinfo").getByRole("link", { name: link, exact: true }).click();

        await expectPage(page, path);
        await expect(page.getByRole("heading", { name: heading, exact: true, level: 1 })).toBeVisible();
    });

// Venatus loads Google's consent dialog. This stands in for it with a queue that runs
// what is pushed at once and a dialog that counts how often it is opened.
test("the footer's cookie settings open the consent dialog again", async ({ page }) => {
    await page.route(url => url.hostname === "hb.vntsm.com", route => route.abort());
    await page.addInitScript(`
        self.consentOpened = 0;
        self.googlefc = {
            callbackQueue: { push: callback => callback() },
            showRevocationMessage: () => { self.consentOpened += 1; },
        };
    `);
    await page.goto("/games/apex-legends");
    await expectPage(page, "/games/apex-legends");

    await page.getByRole("contentinfo").getByRole("button", { name: "Cookie settings" }).click();

    await expect.poll(() => page.evaluate(() => (self as unknown as { consentOpened: number }).consentOpened)).toBe(1);
});

// The inbox fills the window below the header, with nothing under it.
test("the inbox has no footer", async ({ page }) => {
    await signIn(page, "apex-legends@example.com");
    await page.goto("/messages");

    await expectPage(page, "/messages");
    await expect(page.getByRole("contentinfo")).toHaveCount(0);
});

test("back and forward go through the pages opened, once each", async ({ page }) => {
    await page.goto("/games/apex-legends");
    const logo = page.getByRole("link", { name: "TeamTavern" });

    await logo.click();
    await expectPage(page, "/");
    await logo.click();

    await page.goBack();
    await expectPage(page, "/games/apex-legends");
    await expect(page).toHaveURL(url => url.pathname === "/games/apex-legends");
    await expect(page.getByRole("heading", feedHeading)).toBeVisible();

    await page.goForward();
    await expectPage(page, "/");
    await expect(page).toHaveURL(url => url.pathname === "/");
    await expect(page.getByRole("heading", feedHeading)).toHaveCount(0);
});
