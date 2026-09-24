import { expect, Page } from "@playwright/test";
import { expectPage } from "./pages";

// Every seeded account shares this password (`stacks/test-seed/players.sql`).
export const password = "tester-password";

// The suite shares one database, so every player a test creates carries a suffix unique to
// the run and to the test, and a retry does not collide with what a failed attempt left.
let suffixes = 0;
export function unique(prefix: string): string {
    suffixes += 1;
    return `${prefix}${Date.now().toString(36)}${suffixes}`;
}

export async function submitPasswordSignIn(page: Page, emailOrNickname: string, password_ = password) {
    await page.goto("/signin");
    await page.getByLabel("Email or nickname").fill(emailOrNickname);
    await page.getByLabel("Password").fill(password_);
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
}

// Signing in from the sign-in page itself returns to the home page, and the header shows
// the player once the page has changed.
export async function signIn(page: Page, emailOrNickname: string) {
    await submitPasswordSignIn(page, emailOrNickname);
    await expectPage(page, "/");
    await expect(page.getByRole("button", { name: "Account menu" })).toBeVisible();
}

// The account menu is headed by the nickname of the player signed in, the one place the
// header names them.
export async function expectSignedInAs(page: Page, nickname: string) {
    await page.getByRole("button", { name: "Account menu" }).click();
    await expect(page.getByRole("group", { name: nickname })).toBeVisible();
    await page.keyboard.press("Escape");
}

export async function signOut(page: Page) {
    await page.getByRole("button", { name: "Account menu" }).click();
    await page.getByRole("button", { name: "Sign out" }).click();
    await expectPage(page, "/");
    await expect(page.getByRole("link", { name: "Sign in" })).toBeVisible();
}

// A player new to the site, signed up with a password and signed in, landing on the home
// page. Their nickname is unique to the run, and so is their email.
export async function signUp(page: Page, prefix = "P"): Promise<string> {
    const nickname = unique(prefix);
    await page.goto("/signup");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await expectPage(page, "/");
    await expect(page.getByRole("button", { name: "Account menu" })).toBeVisible();
    return nickname;
}
