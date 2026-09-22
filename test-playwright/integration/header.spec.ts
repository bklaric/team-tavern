import { expect, test } from "@playwright/test";
import { signIn } from "../accounts";

test.describe("signed out, the header", () => {
    test("offers New post, Sign in and Sign up, and no inbox or bell", async ({ page }) => {
        await page.goto("/");

        const header = page.getByRole("banner");
        await expect(header.getByRole("link", { name: "New post" })).toHaveAttribute("href", "/post");
        await expect(header.getByRole("link", { name: "Sign in" })).toBeVisible();
        await expect(header.getByRole("link", { name: "Sign up" })).toBeVisible();
        await expect(header.getByRole("link", { name: /^Messages/ })).toHaveCount(0);
        await expect(header.getByRole("button", { name: /^Notifications/ })).toHaveCount(0);
        await expect(header.getByRole("button", { name: "Account menu" })).toHaveCount(0);
    });
});

// The seed gives ValorantTester one Valorant post and GroupTester two, a group and a
// community (`stacks/test-seed/players.sql`).
test.describe("Games", () => {
    test("marks the one game a player has a post in", async ({ page }) => {
        await signIn(page, "ValorantTester");

        await page.getByRole("button", { name: "Games" }).click();

        const games = page.getByRole("dialog", { name: "Games" });
        await expect(games.getByRole("link")).toHaveCount(10);
        await expect(games.getByRole("link", { name: /^Valorant/ })).toContainText("Your post");
        await expect(games.locator(".cover-mark")).toHaveCount(1);
    });

    test("marks a game with several of the player's posts in the plural", async ({ page }) => {
        await signIn(page, "GroupTester");

        await page.getByRole("button", { name: "Games" }).click();

        await expect(page.getByRole("dialog", { name: "Games" }).getByRole("link", { name: /^Valorant/ }))
            .toContainText("Your posts");
    });

    test("opens a game's feed from its cover and closes", async ({ page }) => {
        await page.goto("/");

        await page.getByRole("button", { name: "Games" }).click();
        await page.getByRole("dialog", { name: "Games" }).getByRole("link", { name: "Apex Legends" }).click();

        await expect(page).toHaveURL(url => url.pathname === "/games/apex");
        await expect(page.getByRole("dialog", { name: "Games" })).toHaveCount(0);
    });
});

test.describe("the header's menus", () => {
    test("open one at a time and close on Escape and on a click outside", async ({ page }) => {
        await signIn(page, "NewTester");
        const games = page.getByRole("dialog", { name: "Games" });
        const accountButton = page.getByRole("button", { name: "Account menu" });
        const account = page.getByRole("menu", { name: "NewTester" });

        await page.getByRole("button", { name: "Games" }).click();
        await expect(games).toBeVisible();
        await accountButton.click();
        await expect(account).toBeVisible();
        await expect(games).toHaveCount(0);

        // Opening a menu puts the focus in it, and Escape gives it back to the button.
        await expect(account.getByRole("menuitem", { name: "Your posts" })).toBeFocused();
        await page.keyboard.press("Escape");
        await expect(account).toHaveCount(0);
        await expect(accountButton).toBeFocused();

        await accountButton.click();
        await expect(account).toBeVisible();
        await page.getByRole("heading", { name: "TeamTavern" }).click();
        await expect(account).toHaveCount(0);

        // A second press on the button that opened it closes it.
        await page.getByRole("button", { name: /^Notifications/ }).click();
        await expect(page.getByRole("dialog", { name: "Notifications" })).toContainText("No notifications yet.");
        await page.getByRole("button", { name: /^Notifications/ }).click();
        await expect(page.getByRole("dialog", { name: "Notifications" })).toHaveCount(0);
    });

    test("lead to the inbox and the account page", async ({ page }) => {
        await signIn(page, "NewTester");

        await page.getByRole("link", { name: "Messages" }).click();
        await expect(page).toHaveURL(url => url.pathname === "/messages");
        await expect(page.getByRole("link", { name: "Messages" })).toHaveAttribute("aria-current", "page");

        await page.getByRole("button", { name: "Account menu" }).click();
        await page.getByRole("menuitem", { name: "Account" }).click();
        await expect(page).toHaveURL(url => url.pathname === "/account");
        await expect(page.getByRole("menu", { name: "NewTester" })).toHaveCount(0);
    });
});

test.describe("on a phone, the header", () => {
    test.use({ viewport: { width: 375, height: 740 } });

    test("holds Sign in and Sign up in a menu", async ({ page }) => {
        await page.goto("/");
        await expect(page.getByRole("link", { name: "Sign in" })).toBeHidden();

        await page.getByRole("button", { name: "Menu" }).click();

        const menu = page.getByRole("dialog", { name: "Menu" });
        await expect(menu.getByRole("menuitem", { name: "Sign in" })).toBeVisible();
        await menu.getByRole("menuitem", { name: "Sign up" }).click();
        await expect(page).toHaveURL(url => url.pathname === "/signup");
        await expect(menu).toHaveCount(0);
    });

    test("opens the account menu as a sheet from the bottom", async ({ page }) => {
        await signIn(page, "NewTester");

        await page.getByRole("button", { name: "Account menu" }).click();

        const sheet = page.getByRole("dialog", { name: "NewTester" });
        await expect(sheet).toHaveClass(/overlay-bottom/);
        await expect(sheet.getByRole("menuitem", { name: "Sign out" })).toBeVisible();
        await sheet.getByRole("button", { name: "Close" }).click();
        await expect(sheet).toHaveCount(0);
    });
});
