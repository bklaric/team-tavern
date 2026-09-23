import { expect, Locator, Page, test } from "@playwright/test";
import { password, signIn, signOut, unique } from "../accounts";
import { discordUser, fakeDiscord } from "../discord";
import { expectPage } from "../pages";

// The seeded posts (`stacks/test-seed/players.sql`) ask to be reached every way a post can.
// In Valorant, ValorantTester's player post is happy either way, GroupTester's group Night
// Owls prefers messages and their community Radiant Rising is joined on Discord. In Team
// Fortress 2, TeamFortress2Tester would rather be added off-site and CommunityTester's
// Payload Pals is joined through its website. Every account holds every contact. The
// panels opened here leave Night Owls alone, whose owner's page post-page.spec reads for
// no reveals.

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

const panel = (page: Page, name: string) => page.getByRole("dialog", { name });

// The panel from the top: section headings, the rules between them, a community's join
// button and the rows' labels.
const panelOrder = (dialog: Locator) =>
    dialog.locator(".panel-section > h3, .rule, .panel-section > .button, .contact-label");

async function openFeed(page: Page, handle: string) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

// The panel of the post the card names, once its contacts are shown.
async function openPanel(page: Page, name: string): Promise<Locator> {
    await card(page, name).locator(".card-contact").click();
    const dialog = panel(page, name);
    await expect(dialog.locator(".contact-rows")).toBeVisible();
    return dialog;
}

test.describe("the contact panel", () => {
    test("is opened by a button saying how the post asks to be reached", async ({ page }) => {
        await openFeed(page, "valorant");
        await expect(card(page, "ValorantTester").locator(".card-contact")).toHaveText("Contact");
        await expect(card(page, "Night Owls").locator(".card-contact")).toHaveText("Message");
        await expect(card(page, "Radiant Rising").locator(".card-contact")).toHaveText("Join Discord");

        await openFeed(page, "team-fortress-2");
        await expect(card(page, "TeamFortress2Tester").locator(".card-contact")).toHaveText("Add on Discord");
        await expect(card(page, "Payload Pals").locator(".card-contact")).toHaveText("Visit site");
    });

    test("puts the message box first for a player happy either way", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "valorant");
        const dialog = await openPanel(page, "ValorantTester");

        await expect(dialog.locator(".panel-title p")).toHaveText(/^Valorant player · Active /);
        await expect(panelOrder(dialog)).toHaveText(
            ["Message ValorantTester", "or add ValorantTester off-site", "Discord", "Riot ID"]);
        await expect(dialog.locator(".contact-value")).toHaveText(["ValorantTester", "ValorantTester#EUW"]);
        await expect(dialog.getByText(
            "Your message starts a conversation about ValorantTester's post. Replies show up here and in your inbox."))
            .toBeVisible();
        await expect(dialog.getByRole("textbox", { name: "Message" })).toBeDisabled();
        await expect(dialog.getByRole("button", { name: "Send" })).toHaveClass(/button-primary/);
    });

    test("puts the contacts first for a player who'd rather be added, and outlines Send", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "team-fortress-2");
        const dialog = await openPanel(page, "TeamFortress2Tester");

        await expect(panelOrder(dialog)).toHaveText(
            ["Prefers Discord", "Discord", "Steam profile", "or message on TeamTavern"]);
        await expect(dialog.getByRole("button", { name: "Send" })).toHaveClass(/button-outline/);
    });

    test("makes a community's invite or website the one filled button", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "valorant");
        const discord = await openPanel(page, "Radiant Rising");

        await expect(discord.locator(".panel-title p")).toHaveText("Valorant community · Run by GroupTester");
        await expect(panelOrder(discord)).toHaveText(
            ["Join on Discord", "Open the invite", "Discord invite", "Website", "or message on TeamTavern"]);
        await expect(discord.getByRole("link", { name: "Open the invite" }))
            .toHaveAttribute("href", "https://discord.gg/radiantrising");
        await page.keyboard.press("Escape");

        await openFeed(page, "team-fortress-2");
        const website = await openPanel(page, "Payload Pals");

        await expect(panelOrder(website)).toHaveText(
            ["Join on their website", "Visit site", "Website", "or message on TeamTavern"]);
        // The owner left the scheme out.
        await expect(website.getByRole("link", { name: "Visit site" }))
            .toHaveAttribute("href", "https://payloadpals.example.com");
    });

    test("copies a contact", async ({ page, context }) => {
        await context.grantPermissions(["clipboard-read", "clipboard-write"]);
        await signIn(page, "new@example.com");
        await openFeed(page, "valorant");
        const dialog = await openPanel(page, "ValorantTester");

        const copy = dialog.getByRole("button", { name: "Copy Riot ID" });
        await copy.click();

        await expect(copy).toHaveText("Copied");
        expect(await page.evaluate(() => navigator.clipboard.readText())).toBe("ValorantTester#EUW");
        await expect(copy).toHaveText("Copy");
    });

    test("opens from a post's page, and gives the focus back when it closes", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "valorant");
        await card(page, "ValorantTester").getByRole("link", { name: "ValorantTester" }).click();
        await expectPage(page, /^\/games\/valorant\/posts\/\d+$/);

        const contact = page.getByRole("button", { name: "Contact" });
        await contact.click();
        await expect(panel(page, "ValorantTester")).toBeVisible();
        await page.keyboard.press("Escape");

        await expect(panel(page, "ValorantTester")).toHaveCount(0);
        await expect(contact).toBeFocused();
    });

    test("sends a visitor to sign up, and back to the open panel", async ({ page }) => {
        await openFeed(page, "valorant");
        await card(page, "ValorantTester").locator(".card-contact").click();

        await expectPage(page, "/signup");
        await expect(page.getByText(
            "Messages and contacts need an account. You'll come straight back to ValorantTester's post."))
            .toBeVisible();
        const nickname = unique("C");
        await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
        await page.getByLabel("Nickname").fill(nickname);
        await page.getByLabel("Password").fill(password);
        await page.getByRole("button", { name: "Create account" }).click();

        await expectPage(page, "/games/valorant");
        await expect(panel(page, "ValorantTester").locator(".contact-rows")).toBeVisible();
        await expect(page).toHaveURL(url => !url.searchParams.has("contact"));
    });

    test("sends a visitor who signs up with Discord back to the open panel", async ({ page }) => {
        await fakeDiscord(page, discordUser(`${unique("c")}@example.com`, true));
        await openFeed(page, "valorant");
        await card(page, "Radiant Rising").locator(".card-contact").click();
        await expectPage(page, "/signup");
        await expect(page.getByText("You'll come straight back to GroupTester's post.", { exact: false })).toBeVisible();

        await page.getByRole("button", { name: "Continue with Discord" }).click();
        await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
        await page.getByLabel("Nickname").fill(unique("D"));
        await page.getByRole("button", { name: "Continue" }).click();

        await expectPage(page, "/games/valorant");
        await expect(panel(page, "Radiant Rising").locator(".contact-rows")).toBeVisible();
    });

    test("counts on the owner's home page each time it shows the contacts", async ({ page }) => {
        await signIn(page, "new@example.com");
        await openFeed(page, "apex-legends");
        await openPanel(page, "ApexLegendsTester");
        await page.keyboard.press("Escape");
        await signOut(page);

        await signIn(page, "apex-legends@example.com");
        await expect(page.getByText("Contacts shown 1 time")).toBeVisible();
    });
});
