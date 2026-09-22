import { expect, Page } from "@playwright/test";

// A link writes the new location at once and draws its page on a later tick, so for a
// moment the URL names a page that isn't showing yet. The router marks the page it has
// drawn with its path, and that is what a test waits on before acting on the page.
export async function expectPage(page: Page, path: string | RegExp) {
    await expect(page.locator("#spa-teamtavern > [data-path]")).toHaveAttribute("data-path", path);
}
