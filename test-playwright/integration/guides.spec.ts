import { expect, test } from "@playwright/test";
import { expectPage } from "../pages";

const guidePath = "/guides/join-an-esports-team";
const guideHeading = "How to join an esports team";

test("the guides list each guide with its last update and lead to it", async ({ page }) => {
    await page.goto("/guides");
    await expectPage(page, "/guides");
    const entry = page.getByRole("listitem").filter({ has: page.getByRole("link", { name: guideHeading }) });
    await expect(entry).toContainText(/Updated \d{1,2} [A-Z][a-z]+ \d{4}/);

    await entry.getByRole("link", { name: guideHeading }).click();

    await expectPage(page, guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();
    await expect(page.getByText(/^Last updated \d{1,2} [A-Z][a-z]+ \d{4}$/)).toBeVisible();
    await expect(page).toHaveTitle(/^How to join an esports team/);
});

test("the guides show the cover of the game each is about, and the logo for the rest", async ({ page }) => {
    await page.goto("/guides");
    await expectPage(page, "/guides");
    const image = (heading: string) =>
        page.getByRole("listitem").filter({ has: page.getByRole("link", { name: heading }) }).locator("img");

    await expect(image("Deadlock team size, party size and playing ranked with friends"))
        .toHaveAttribute("src", "/images/games/400/deadlock.webp");
    await expect(image(guideHeading)).toHaveAttribute("src", "/logo-mark.svg");
    for (const cover of await page.locator(".guide-list img").all())
        await expect.poll(() => cover.evaluate((image: HTMLImageElement) => image.complete && image.naturalWidth > 0))
            .toBe(true);
});

test("a guide's cover leads to the guide", async ({ page }) => {
    const deadlockHeading = "Deadlock team size, party size and playing ranked with friends";
    await page.goto("/guides");
    await expectPage(page, "/guides");

    await page.getByRole("listitem").filter({ has: page.getByRole("link", { name: deadlockHeading }) })
        .locator("img").click();

    await expectPage(page, "/guides/deadlock-ranked-with-friends");
    await expect(page.getByRole("heading", { name: deadlockHeading, level: 1 })).toBeVisible();
});

// The guide's text is HTML from the server, and its links to the site's pages move
// within the site as any other in-app link does, without loading it again.
test("a guide's link to a feed opens the feed without a reload", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();
    await page.evaluate(() => { (self as unknown as { stayed: boolean }).stayed = true; });

    await page.getByRole("link", { name: "Valorant players and groups on TeamTavern" }).click();

    await expectPage(page, "/games/valorant");
    await expect(page.getByRole("heading", { name: "Valorant", level: 1 })).toBeVisible();
    expect(await page.evaluate(() => (self as unknown as { stayed?: boolean }).stayed)).toBe(true);
});

test("the guides to joining and making a team lead to each other", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "How to make an esports team" }).click();

    await expectPage(page, "/guides/make-an-esports-team");
    await expect(page.getByRole("heading", { name: "How to make an esports team", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "chart in the join guide" }).click();

    await expectPage(page, guidePath);
    await expect(page.getByRole("figure", { name: /^Groups asking for each role/ })).toBeInViewport();
});

test("the guide to joining a team leads to the Marvel Rivals guide, and that to its game's feed", async ({ page }) => {
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "How to build a Marvel Rivals Championship team" }).click();

    await expectPage(page, "/guides/marvel-rivals-championship-team");
    await expect(page.getByRole("heading", { name: "How to build a Marvel Rivals Championship team", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "Marvel Rivals players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/marvel-rivals");
    await expect(page.getByRole("heading", { name: "Marvel Rivals", level: 1 })).toBeVisible();
});

test("the guide to joining a team leads to the Rocket League guide, and that to its game's feed", async ({ page }) => {
    const rocketLeagueHeading = "Rocket League tournament times, rules and finding a partner";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: rocketLeagueHeading }).click();

    await expectPage(page, "/guides/rocket-league-tournaments");
    await expect(page.getByRole("heading", { name: rocketLeagueHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "Rocket League players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/rocket-league");
    await expect(page.getByRole("heading", { name: "Rocket League", level: 1 })).toBeVisible();
});

test.describe("in Tokyo, which keeps one time all year", () => {
    test.use({ timezoneId: "Asia/Tokyo", locale: "en-US" });

    test("the Rocket League guide gives its UTC times in the reader's time", async ({ page }) => {
        await page.goto("/guides/rocket-league-tournaments");

        const northAmerica = page.getByRole("row").filter({ hasText: "North America" });
        await expect(northAmerica.getByRole("cell").nth(1)).toHaveText("7am, 11am");
        await expect(page.getByText(
            "Times on this page are converted from Epic's UTC to your time zone, Japan Standard Time.")).toBeVisible();
    });
});

test.describe("in a browser that reports UTC", () => {
    test.use({ timezoneId: "UTC", locale: "en-GB" });

    test("the Rocket League guide keeps its UTC times", async ({ page }) => {
        await page.goto("/guides/rocket-league-tournaments");

        const northAmerica = page.getByRole("row").filter({ hasText: "North America" });
        await expect(northAmerica.getByRole("cell").nth(1)).toHaveText("22:00, 02:00");
        await expect(page.getByText("Times on this page are in UTC.")).toBeVisible();
    });
});

test("the guide to joining a team leads to The Finals guide, and that to its game's feed", async ({ page }) => {
    const theFinalsHeading = "How to play ranked with friends in The Finals";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: theFinalsHeading }).click();

    await expectPage(page, "/guides/the-finals-ranked-with-friends");
    await expect(page.getByRole("heading", { name: theFinalsHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "The Finals players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/the-finals");
    await expect(page.getByRole("heading", { name: "The Finals", level: 1 })).toBeVisible();
});

test("the guide to joining a team leads to the Deadlock guide, and that to its game's feed", async ({ page }) => {
    const deadlockHeading = "Deadlock team size, party size and playing ranked with friends";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: deadlockHeading }).click();

    await expectPage(page, "/guides/deadlock-ranked-with-friends");
    await expect(page.getByRole("heading", { name: deadlockHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("figure", { name: /^How much more often a duo wins than a solo player/ })).toBeVisible();

    await page.getByRole("link", { name: "Deadlock players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/deadlock");
    await expect(page.getByRole("heading", { name: "Deadlock", level: 1 })).toBeVisible();
});

test("the guide to joining a team leads to the Rainbow Six Siege guide, and that to its game's feed", async ({ page }) => {
    const siegeHeading = "How to play ranked with friends in Rainbow Six Siege";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: siegeHeading }).click();

    await expectPage(page, "/guides/rainbow-six-siege-ranked-with-friends");
    await expect(page.getByRole("heading", { name: siegeHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("table")).toContainText("Gold to Diamond");

    await page.getByRole("link", { name: "Rainbow Six Siege players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/rainbow-six-siege");
    await expect(page.getByRole("heading", { name: "Rainbow Six Siege", level: 1 })).toBeVisible();
});

test("the guide to joining a team leads to the Marvel Rivals ranked guide, and that to its game's feed", async ({ page }) => {
    const rivalsHeading = "Marvel Rivals team size, party size and playing ranked with friends";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: rivalsHeading }).click();

    await expectPage(page, "/guides/marvel-rivals-ranked-with-friends");
    await expect(page.getByRole("heading", { name: rivalsHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("table")).toContainText("Gold II reaches Bronze II or Silver II up to Platinum II");

    await page.getByRole("link", { name: "Marvel Rivals players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/marvel-rivals");
    await expect(page.getByRole("heading", { name: "Marvel Rivals", level: 1 })).toBeVisible();
});

test("the guide to joining a team leads to the Overwatch ranked guide, and that to its game's feed", async ({ page }) => {
    const overwatchHeading = "Overwatch team size, party size and playing ranked with friends";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: overwatchHeading }).click();

    await expectPage(page, "/guides/overwatch-ranked-with-friends");
    await expect(page.getByRole("heading", { name: overwatchHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("table")).toContainText("Gold 3: Silver 3 to Platinum 3");

    await page.getByRole("link", { name: "Overwatch players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/overwatch");
    await expect(page.getByRole("heading", { name: "Overwatch", level: 1 })).toBeVisible();
});

test("the Overwatch ranked guide leads to the Overwatch section of the guide to joining a team", async ({ page }) => {
    await page.goto("/guides/overwatch-ranked-with-friends");
    await expect(page.getByRole("heading", { name: "Overwatch team size, party size and playing ranked with friends", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "joining an Overwatch team" }).click();

    await expectPage(page, "/guides/join-an-esports-team");
    await expect(page.getByRole("heading", { name: "Overwatch", exact: true, level: 3 })).toBeInViewport();
});

test("the guide to joining a team leads to the Valorant ranked guide, and that to its game's feed", async ({ page }) => {
    const valorantHeading = "Valorant team size, party size and playing ranked with friends";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: valorantHeading }).click();

    await expectPage(page, "/guides/valorant-ranked-with-friends");
    await expect(page.getByRole("heading", { name: valorantHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("table")).toContainText("Gold 3 with Diamond 1: no");

    await page.getByRole("link", { name: "Valorant players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/valorant");
    await expect(page.getByRole("heading", { name: "Valorant", level: 1 })).toBeVisible();
});

test("the Valorant ranked guide leads to the Premier section of the guide to joining a team", async ({ page }) => {
    await page.goto("/guides/valorant-ranked-with-friends");
    await expect(page.getByRole("heading", { name: "Valorant team size, party size and playing ranked with friends", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "Joining a Premier team", exact: true }).click();

    await expectPage(page, "/guides/join-an-esports-team");
    await expect(page.getByRole("heading", { name: "Valorant: Premier", level: 3 })).toBeInViewport();
});

test("the guide to joining a team leads to the FNCS guide, and that to its game's feed", async ({ page }) => {
    const fncsHeading = "FNCS 2027 is Trios: how to find a trio and qualify";
    await page.goto(guidePath);
    await expect(page.getByRole("heading", { name: guideHeading, level: 1 })).toBeVisible();

    await page.getByRole("link", { name: fncsHeading }).click();

    await expectPage(page, "/guides/fncs-team");
    await expect(page.getByRole("heading", { name: fncsHeading, level: 1 })).toBeVisible();
    await expect(page.getByRole("table")).toContainText("350 or above");

    await page.getByRole("link", { name: "Fortnite players and groups on TeamTavern" }).last().click();

    await expectPage(page, "/games/fortnite");
    await expect(page.getByRole("heading", { name: "Fortnite", level: 1 })).toBeVisible();
});

test("the Marvel Rivals guides lead to each other", async ({ page }) => {
    await page.goto("/guides/marvel-rivals-championship-team");
    await expect(page.getByRole("heading", { name: "How to build a Marvel Rivals Championship team", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "PC, PlayStation and Xbox" }).click();

    await expectPage(page, "/guides/marvel-rivals-ranked-with-friends");
    await expect(page.getByRole("heading", { name: "PC, PlayStation and Xbox", level: 2 })).toBeInViewport();

    await page.getByRole("link", { name: "recruiting a Faction" }).click();

    await expectPage(page, "/guides/marvel-rivals-championship-team");
    await expect(page.getByRole("heading", { name: "Recruiting your Faction", level: 2 })).toBeInViewport();
});

test("the Rainbow Six Siege guide leads to the role chart in the guide to joining a team", async ({ page }) => {
    await page.goto("/guides/rainbow-six-siege-ranked-with-friends");
    await expect(page.getByRole("heading", { name: "How to play ranked with friends in Rainbow Six Siege", level: 1 })).toBeVisible();

    await page.getByRole("link", { name: "the role chart in the guide to joining a team" }).click();

    await expectPage(page, guidePath);
    await expect(page.getByRole("heading", { name: "Pick one game and one role, ideally one teams are short of", level: 2 })).toBeInViewport();
});

// The guide's text comes after the page loads, so the page, not the browser,
// finds the part of it the address names.
test("an address naming a part of a guide opens the guide at it", async ({ page }) => {
    await page.goto(`${guidePath}#from-amateur-team-to-pro`);

    await expect(page.getByRole("heading", { name: "From amateur team to pro", level: 2 })).toBeInViewport();
    expect(await page.evaluate(() => scrollY)).toBeGreaterThan(0);
});

test("a guide that doesn't exist is not found", async ({ page }) => {
    await page.goto("/guides/no-such-guide");

    await expectPage(page, "/guides/no-such-guide");
    await expect(page.getByRole("heading", { name: "Page could not be found.", level: 1 })).toBeVisible();
});
