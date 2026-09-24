import { Browser, devices, expect, Page } from "@playwright/test";

// A link writes the new location at once and draws its page on a later tick, so for a
// moment the URL names a page that isn't showing yet. The router marks the page it has
// drawn with its path, and that is what a test waits on before acting on the page.
export async function expectPage(page: Page, path: string | RegExp) {
    await expect(page.locator("#spa-teamtavern > [data-path]")).toHaveAttribute("data-path", path);
}

// A post's page, found as a player finds it: by its name on its game's feed. It opens a
// context of its own, so a test whose visitor isn't a browser can still ask. The runner
// gives a context opened by hand the calling test's options, a bot's user agent among them,
// so this one says what it is.
export async function postPath(browser: Browser, baseURL: string, feedPath: string, name: string): Promise<string> {
    const context = await browser.newContext({
        ...devices["Desktop Chrome"], baseURL, javaScriptEnabled: true, extraHTTPHeaders: {},
    });
    try {
        const page = await context.newPage();
        await page.goto(feedPath);
        await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
        const href = await page.locator(".card").getByRole("link", { name, exact: true }).getAttribute("href");
        expect(href).toMatch(/^\/games\/[^/]+\/posts\/\d+$/);
        return href!;
    } finally {
        await context.close();
    }
}
