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

test("the footer leads to the privacy policy", async ({ page }) => {
    await page.goto("/games/apex-legends");

    await page.getByRole("contentinfo").getByRole("link", { name: "Privacy" }).click();

    await expectPage(page, "/privacy");
    await expect(page.getByRole("heading", { name: "Privacy Policy", level: 1 })).toBeVisible();
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
