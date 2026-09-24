import { expect, Locator, Page } from "@playwright/test";

// The test stack's mail service keeps what the site sends, and shows an address's mail at
// /mail on the site's origin, newest first, each email in a frame whose links open in the
// tab.

export async function openMail(page: Page, address: string) {
    await page.goto(`/mail?to=${encodeURIComponent(address)}`);
    await expect(page.getByRole("heading", { name: `Mail to ${address}` })).toBeVisible();
}

export const emails = (page: Page, subject: string) =>
    page.getByRole("article").filter({ has: page.getByRole("heading", { name: subject, exact: true }) });

export const body = (email: Locator) => email.frameLocator("iframe");
