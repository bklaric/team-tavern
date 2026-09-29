import { expect, Page, test } from "@playwright/test";
import { signIn, submitPasswordSignIn } from "../accounts";

// The server logs only its own failures, so a page reports to it the ones it didn't expect.
// What a report carries is what reaches the log, so each test reads the reports the page sent.
function collectReports(page: Page): unknown[] {
    const reports: unknown[] = [];
    page.on("request", request => {
        if (new URL(request.url()).pathname === "/api/client/errors")
            reports.push(request.postDataJSON());
    });
    return reports;
}

test("a response the route doesn't declare is reported, by paths without their query strings", async ({ page }) => {
    const reports = collectReports(page);
    await page.route(url => url.pathname === "/api/games/valorant", route => route.fulfill({ status: 418, body: "" }));

    await page.goto("/games/valorant?nonce=secret");

    await expect(page.getByText("There has been an error loading the game.")).toBeVisible();
    await expect.poll(() => reports).toContainEqual({
        page: "/games/valorant",
        request: "GET /api/games/valorant",
        failure: "The route declares no response with status 418.",
        bundle: expect.stringMatching(/^app\.min\.[^/]+\.js$/),
    });
});

// The feed asks for a signed-in player's own posts, and doesn't expect to be told nobody is
// signed in when the header has just said somebody is.
test("a failure the page doesn't handle by name is reported", async ({ page }) => {
    await signIn(page, "new@example.com");
    const reports = collectReports(page);
    await page.route(url => url.pathname === "/api/games/valorant/own", route => route.fulfill({ status: 401, body: "" }));

    await page.goto("/games/valorant");

    await expect.poll(() => reports).toContainEqual(expect.objectContaining({
        page: "/games/valorant",
        request: "GET /api/games/valorant/own",
        failure: "notAuthorized",
    }));
});

// Signed out, the header and the home page are both told nobody is signed in, and a wrong
// password is told it is wrong, all of which the pages show as they should.
test("a failure the page handles is not reported", async ({ page }) => {
    const reports = collectReports(page);

    await page.goto("/");
    await expect(page.locator(".cover-grid .cover")).toHaveCount(11);
    await submitPasswordSignIn(page, "NewTester", "wrong-password");
    await expect(page.getByText("Entered password is incorrect.")).toBeVisible();

    // A report goes out as its failure comes in, so once the network is quiet there is none
    // still to come.
    await page.waitForLoadState("networkidle");
    expect(reports).toEqual([]);
});
