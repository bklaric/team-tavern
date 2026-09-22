import { expect, test } from "@playwright/test";

// A feed is headed by its game's title.
const feedHeading = { name: "Apex Legends", level: 1 };

test("a link clicked with Ctrl or Cmd opens in a new tab and leaves this one", async ({ page }) => {
    await page.goto("/games/apex");

    const [tab] = await Promise.all([
        page.context().waitForEvent("page"),
        page.getByRole("banner").getByRole("link", { name: "Sign in" }).click({ modifiers: ["ControlOrMeta"] }),
    ]);

    await expect(tab).toHaveURL(url => url.pathname === "/signin");
    await expect(page).toHaveURL(url => url.pathname === "/games/apex");
    await expect(page.getByRole("heading", feedHeading)).toBeVisible();
});

test("back and forward go through the pages opened, once each", async ({ page }) => {
    await page.goto("/games/apex");
    const logo = page.getByRole("link", { name: "TeamTavern" });

    await logo.click();
    await expect(page).toHaveURL(url => url.pathname === "/");
    await logo.click();

    await page.goBack();
    await expect(page).toHaveURL(url => url.pathname === "/games/apex");
    await expect(page.getByRole("heading", feedHeading)).toBeVisible();

    await page.goForward();
    await expect(page).toHaveURL(url => url.pathname === "/");
    await expect(page.getByRole("heading", feedHeading)).toHaveCount(0);
});
