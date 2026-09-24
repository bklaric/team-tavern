import { expect, Page, test } from "@playwright/test";
import { password, unique } from "../accounts";
import { expectPage } from "../pages";

// Every page and overlay fits a 375 px phone, with the longest names a player can give:
// a 40 character nickname and a 50 character group name, neither with a space to break
// at. Their posts go into League of Legends as an Iron IV support, clear of the Top laner
// from Gold IV to Platinum I that `post.spec.ts` fits LeagueOfLegendsTester to.

test.use({ viewport: { width: 375, height: 740 } });

const handle = "league-of-legends";

// What spills: an element reaching past either side of the window, unless a box inside the
// window clips it, as a line cut with an ellipsis is; or one whose content, a long word
// say, runs past the window from inside it, or scrolls sideways inside it.
async function expectFits(page: Page) {
    const spills = await page.evaluate(() => {
        const width = document.documentElement.clientWidth;
        const inside = (rect: DOMRect) => rect.left >= -1 && rect.right <= width + 1;
        const clipped = (element: Element) => {
            for (let parent = element.parentElement; parent && parent !== document.body; parent = parent.parentElement) {
                const overflow = getComputedStyle(parent).overflowX;
                if (overflow !== "visible" && inside(parent.getBoundingClientRect())) {
                    return true;
                }
            }
            return false;
        };
        const found: string[] = [];
        for (const element of document.querySelectorAll("#spa-teamtavern *")) {
            const rect = element.getBoundingClientRect();
            if (rect.width === 0 || rect.height === 0 || element.closest(".visually-hidden")) {
                continue;
            }
            const outside = !inside(rect) && !clipped(element);
            const overflow = getComputedStyle(element).overflowX;
            const scrolls = overflow === "auto" || overflow === "scroll";
            const overrun = overflow !== "hidden" && overflow !== "clip"
                && element.clientWidth > 0 && element.scrollWidth > element.clientWidth + 1
                && (scrolls || rect.left + element.scrollWidth > width + 1);
            if (outside || overrun) {
                const text = (element.textContent ?? "").trim().slice(0, 40);
                found.push(`<${element.tagName.toLowerCase()} class="${element.getAttribute("class") ?? ""}"> ${text}`);
            }
        }
        return found;
    });
    expect(spills).toEqual([]);
}

const settled = (page: Page) => expect(page.locator("[aria-busy=true]")).toHaveCount(0);

async function visit(page: Page, path: string) {
    await page.goto(path);
    await expectPage(page, path.split(/[?#]/)[0]);
    await settled(page);
    await expectFits(page);
}

// The longest nickname, unique to the run.
function longNickname(): string {
    return unique("L").padEnd(40, "x");
}

async function signUpAs(page: Page, nickname: string) {
    await page.goto("/signup");
    await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
    await page.getByLabel("Nickname").fill(nickname);
    await page.getByLabel("Password").fill(password);
    await page.getByRole("button", { name: "Create account" }).click();
    await expectPage(page, "/");
    await expect(page.getByRole("button", { name: "Account menu" })).toBeVisible();
}

const pill = (page: Page, group: string, text: string) =>
    page.getByRole("group", { name: group }).locator(".pill", { hasText: new RegExp(`^${text}$`) });

const field = (page: Page, label: string) =>
    page.locator(".field").filter({ has: page.locator(".field-label", { hasText: new RegExp(`^${label}$`) }) });

async function publishPlayer(page: Page) {
    await page.goto(`/games/${handle}/post/player`);
    await page.getByLabel("Rank", { exact: true }).selectOption("iron-iv");
    await pill(page, "Role", "Support").click();
    await page.getByRole("button", { name: "Publish post" }).click();
    await expectPage(page, `/games/${handle}/post/player/live`);
    await settled(page);
    await expectFits(page);
}

async function publishGroup(page: Page, name: string) {
    await page.goto(`/games/${handle}/post/group`);
    await page.getByLabel("Group name").fill(name);
    await pill(page, "Roles you need", "Support").click();
    await field(page, "Rank range").getByLabel("Lowest").selectOption("iron-iv");
    await field(page, "Rank range").getByLabel("Highest").selectOption("iron-iv");
    await page.getByRole("button", { name: "Publish post" }).click();
    await expectPage(page, `/games/${handle}/post/group/live`);
    await settled(page);
    await expectFits(page);
}

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

test.describe("on a phone", () => {
    test("every page fits signed out", async ({ page }) => {
        for (const path of ["/", `/games/${handle}`, "/signin", "/signup", "/forgot-password", "/privacy",
            "/post", "/post/player", "/messages", "/no-such-page"]) {
            await visit(page, path);
        }

        await page.goto(`/games/${handle}`);
        await settled(page);
        await page.locator(".description-summary").click();
        await expect(page.getByRole("dialog", { name: "Tell us about you" })).toBeVisible();
        await expectFits(page);
        await page.keyboard.press("Escape");

        await page.getByRole("button", { name: "Games" }).click();
        await expect(page.getByRole("dialog", { name: "Games" })).toBeVisible();
        await expectFits(page);
        await page.keyboard.press("Escape");

        await page.getByRole("button", { name: "Menu" }).click();
        await expect(page.getByRole("dialog", { name: "Menu" })).toBeVisible();
        await expectFits(page);
    });

    test("every page fits the longest names", async ({ browser }) => {
        test.setTimeout(120_000);
        const owner = await (await browser.newContext()).newPage();
        const nickname = longNickname();
        const groupName = unique("G").padEnd(50, "x");
        await signUpAs(owner, nickname);
        await publishPlayer(owner);
        await publishGroup(owner, groupName);

        // Someone else reads the two posts and writes to the player post's owner.
        const reader = await (await browser.newContext()).newPage();
        await signUpAs(reader, longNickname());
        await visit(reader, `/games/${handle}`);
        await card(reader, groupName).getByRole("link", { name: groupName, exact: true }).click();
        await settled(reader);
        await expectFits(reader);

        await reader.goBack();
        await expectPage(reader, `/games/${handle}`);
        await card(reader, nickname).locator(".card-contact").click();
        const panel = reader.getByRole("dialog", { name: nickname });
        await expect(panel.getByRole("textbox", { name: "Message" })).toBeVisible();
        await expectFits(reader);
        await panel.getByRole("textbox", { name: "Message" }).fill("Duo tonight?");
        await panel.getByRole("button", { name: "Send" }).click();
        await expect(panel.locator(".message-own")).toHaveCount(1);
        await expectFits(reader);

        await panel.getByRole("button", { name: "More" }).click();
        await expectFits(reader);
        await panel.getByRole("group", { name: "More" }).getByRole("button", { name: `Block ${nickname}` }).click();
        await expect(panel.getByRole("alertdialog", { name: `Block ${nickname}?` })).toBeVisible();
        await expectFits(reader);
        await panel.getByRole("button", { name: "Cancel" }).click();
        await panel.getByRole("button", { name: "More" }).click();
        await panel.getByRole("group", { name: "More" }).getByRole("button", { name: "Report this post" }).click();
        await expect(panel.getByRole("heading", { name: `Report ${nickname}` })).toBeVisible();
        await expectFits(reader);
        await reader.keyboard.press("Escape");
        await reader.keyboard.press("Escape");

        await visit(reader, "/messages");
        await reader.locator(".inbox-row").first().click();
        await expect(reader.locator("#spa-teamtavern > [data-path^='/messages/']")).toHaveCount(1);
        await settled(reader);
        await expectFits(reader);

        // The owner, across everything that names them or their posts.
        for (const path of ["/", `/games/${handle}`, "/post", "/post/group", `/games/${handle}/post/group`,
            "/messages", "/account"]) {
            await visit(owner, path);
        }
        await owner.goto("/messages");
        await settled(owner);
        await owner.locator(".inbox-row").first().click();
        await expect(owner.locator("#spa-teamtavern > [data-path^='/messages/']")).toHaveCount(1);
        await settled(owner);
        await expectFits(owner);

        await owner.getByRole("button", { name: "Account menu" }).click();
        await expect(owner.getByRole("dialog", { name: nickname })).toBeVisible();
        await expectFits(owner);
        await owner.keyboard.press("Escape");

        await owner.getByRole("button", { name: /^Notifications/ }).click();
        await expect(owner.getByRole("dialog", { name: "Notifications" })).toBeVisible();
        await settled(owner);
        await expectFits(owner);
        await owner.keyboard.press("Escape");

        await visit(owner, "/");
        await owner.getByRole("link", { name: groupName, exact: true }).click();
        await expectPage(owner, /^\/games\/league-of-legends\/posts\/\d+$/);
        await settled(owner);
        await expectFits(owner);

        await visit(owner, "/account");
        await owner.getByRole("button", { name: "Edit" }).click();
        await expectFits(owner);
        await owner.getByRole("button", { name: "Cancel" }).click();
        await owner.getByRole("button", { name: "Delete account" }).click();
        await expectFits(owner);

        // Last, since it parts the two: the reader's blocked list names the owner.
        await visit(reader, `/games/${handle}`);
        await card(reader, nickname).locator(".card-contact").click();
        await panel.getByRole("button", { name: "More" }).click();
        await panel.getByRole("group", { name: "More" }).getByRole("button", { name: `Block ${nickname}` }).click();
        await panel.getByRole("button", { name: `Block ${nickname}` }).click();
        await expect(reader.locator(".toast-text")).toHaveText(`${nickname} is blocked.`);
        await expectFits(reader);
        await visit(reader, "/account");
        await expect(reader.locator(".person-row")).toHaveCount(1);
        await expectFits(reader);
    });
});

// Between a phone and a desktop the header's and the bar's dropdowns hang from buttons with
// less room to their right than they are wide.
test.describe("between a phone and a desktop", () => {
    for (const width of [640, 800]) {
        test(`the dropdowns stay in a ${width} px window`, async ({ page }) => {
            await page.setViewportSize({ width, height: 800 });
            await visit(page, "/games/valorant");

            await page.getByRole("button", { name: "Games" }).click();
            await expect(page.getByRole("dialog", { name: "Games" })).toBeVisible();
            await expectFits(page);
            await page.keyboard.press("Escape");

            const chips = page.locator(".field-chip[aria-haspopup=dialog]");
            for (let i = 0; i < await chips.count(); i++) {
                await chips.nth(i).click();
                await expect(page.locator(".popover")).toBeVisible();
                await expectFits(page);
                await page.keyboard.press("Escape");
            }
        });
    }
});
