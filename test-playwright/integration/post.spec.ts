import { expect, Page, test } from "@playwright/test";
import { password, signOut, signUp, unique } from "../accounts";
import { discordUser, fakeDiscord, signUpWithDiscord } from "../discord";
import { expectPage } from "../pages";

// Posts go into League of Legends, whose feed holds only LeagueOfLegendsTester's seeded
// player post (`stacks/test-seed/players.sql`): Gold I, Top, Casual, Croatia, English.
// Valorant's feed is the one `feed.spec.ts` asserts whole. Every test signs up a player of
// its own, and the posts they publish stay clear of LeagueOfLegendsTester's rank and role
// unless a test means them to fit.
const feedPath = "/games/league-of-legends";

const expectSettled = (page: Page) => expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.locator(".card-name", { hasText: new RegExp(`^${name}$`) }) });

// The field whose label reads the text, for its controls.
const field = (page: Page, label: string) =>
    page.locator(".field").filter({ has: page.locator(".field-label", { hasText: new RegExp(`^${label}$`) }) });

// A pill is its label: the box inside it is hidden, as a player never sees it.
const pill = (page: Page, group: string, text: string) =>
    page.getByRole("group", { name: group }).locator(".pill", { hasText: new RegExp(`^${text}$`) });

// A group of Rift Owls wanting a Top laner from Gold IV to Platinum I, for Casual games:
// LeagueOfLegendsTester fits it.
async function publishGroup(page: Page, name: string) {
    await page.goto("/games/league-of-legends/post/group");
    await expect(page.getByRole("heading", { name: "Tell players about your group" })).toBeVisible();
    await page.getByLabel("Group name").fill(name);
    await pill(page, "Roles you need", "Top").click();
    await field(page, "Rank range").getByLabel("Lowest").selectOption("gold-iv");
    await field(page, "Rank range").getByLabel("Highest").selectOption("platinum-i");
    await pill(page, "Looking for", "Casual").click();
    await page.getByRole("button", { name: "Publish post" }).click();
    await expectPage(page, "/games/league-of-legends/post/group/live");
}

test.describe("posting", () => {
    test("publishes what the feed was told, then shows what comes closest and the feed", async ({ page }) => {
        const nickname = await signUp(page);
        await page.goto(feedPath);
        await expectSettled(page);
        await page.getByRole("button", { name: "Rank", exact: true }).click();
        await page.getByRole("dialog", { name: "Rank" }).getByRole("combobox").selectOption("iron-iv");
        await page.keyboard.press("Escape");
        await page.getByRole("button", { name: "Role", exact: true }).click();
        await page.getByRole("dialog", { name: "Role" }).getByLabel("Support").check();
        await page.keyboard.press("Escape");
        await expectSettled(page);

        await page.getByRole("link", { name: "Publish post" }).click();
        await expectPage(page, "/games/league-of-legends/post/player");
        await expect(page.getByRole("heading", { name: "Tell groups and players about you" })).toBeVisible();
        await expect(page.getByLabel("Rank", { exact: true })).toHaveValue("iron-iv");
        await expect(page.getByRole("group", { name: "Role" }).getByLabel("Support")).toBeChecked();

        // The preview is the card as the feed will show it.
        const preview = page.getByRole("complementary", { name: "Preview" });
        await page.getByLabel("Location").selectOption("Croatia");
        await expect(preview.locator(".card")).toContainText("Iron IV");
        await expect(preview.locator(".card")).toContainText("Croatia");
        await expect(preview.locator(".card-name")).toHaveText(nickname);

        await page.getByRole("button", { name: "Publish post" }).click();

        await expectPage(page, "/games/league-of-legends/post/player/live");
        await expect(page.getByRole("heading", { name: "Your post is live" })).toBeVisible();
        await expect(page.getByRole("heading", { name: "Nobody fits your post yet" })).toBeVisible();
        await expect(page.getByText("These come closest. We'll email you when someone fits.")).toBeVisible();
        await expect(page.locator(".feed-stack .card").first()).toBeVisible();

        await page.getByRole("link", { name: "See all" }).click();
        await expectPage(page, feedPath);
        await expect(page.getByText("Showing what fits your player post.")).toBeVisible();
        await expectSettled(page);
        await expect(card(page, nickname)).toHaveCount(0);

        await page.getByRole("button", { name: "Clear all" }).click();
        await expectSettled(page);
        await expect(card(page, nickname)).toContainText("Iron IV");
    });

    test("counts the players who fit a new group and shows them", async ({ page }) => {
        await signUp(page);
        const name = unique("Rift Owls ");

        await publishGroup(page, name);

        await expect(page.getByRole("heading", { name: "Your post is live" })).toBeVisible();
        await expect(page.getByRole("heading", { name: "1 player fits your group right now" })).toBeVisible();
        await expect(card(page, "LeagueOfLegendsTester")).toHaveCount(1);
        await expect(card(page, "LeagueOfLegendsTester").locator(".fact-fit").first()).toBeVisible();
    });

    test("starts from the type with the game known, and offers the post the player already has", async ({ page }) => {
        await signUp(page);
        const name = unique("Rift Owls ");
        await publishGroup(page, name);

        await page.goto(feedPath);
        await page.getByRole("banner").getByRole("link", { name: "New post" }).click();
        await expectPage(page, "/post");
        await expect(page.locator(".step-context")).toHaveText("League of Legends");
        const groupCard = page.getByRole("link", { name: /^We're a group looking for players/ });
        await expect(groupCard).toContainText("You have one for League of Legends");

        await groupCard.click();
        await expectPage(page, "/games/league-of-legends/post/group");
        await expect(page.getByRole("heading", { name: "You already have a League of Legends group post" })).toBeVisible();
        await expect(card(page, name)).toHaveCount(1);

        await page.getByRole("button", { name: "Edit it" }).click();
        await expect(page.getByRole("heading", { name: "Edit your group post" })).toBeVisible();
        await expect(page.getByLabel("Group name")).toHaveValue(name);
        await expect(page.getByText("Saving renews your post: it stays active for 30 days from today.")).toBeVisible();
        const renamed = unique("Rift Hawks ");
        await page.getByLabel("Group name").fill(renamed);
        await page.getByRole("button", { name: "Save post" }).click();

        await expectPage(page, "/games/league-of-legends/post/group/live");
        await expect(page.getByRole("heading", { name: "Your post is updated" })).toBeVisible();

        // The feed starts from the player's post, which names it.
        await page.goto(feedPath);
        await expect(page.getByText(`Showing what fits ${renamed}, your group post.`)).toBeVisible();
    });

    test("deletes the post the player already has, saying what goes with it", async ({ page }) => {
        await signUp(page);
        const name = unique("Rift Owls ");
        await publishGroup(page, name);

        await page.goto("/games/league-of-legends/post/group");
        await page.getByRole("button", { name: "Delete it" }).click();
        const confirmation = page.getByRole("alertdialog", { name: `Delete ${name}?` });
        await expect(confirmation).toContainText("It has no conversations.");
        await confirmation.getByRole("button", { name: "Delete post" }).click();

        await expect(page.getByRole("heading", { name: "Tell players about your group" })).toBeVisible();
        await expect(page.getByLabel("Group name")).toHaveValue("");

        await page.goto(feedPath);
        await expectSettled(page);
        await expect(card(page, name)).toHaveCount(0);
    });

    test("names what a community is missing and publishes nothing", async ({ page }) => {
        await signUp(page);
        await page.goto("/games/league-of-legends/post/community");

        await page.getByRole("button", { name: "Publish post" }).click();

        await expect(page.getByText("Give your community a name.")).toBeVisible();
        await expect(page.getByText("Tell players what your community is about.")).toBeVisible();
        await expect(page.getByText("Add your invite, or choose another way to join.")).toBeVisible();
        await expect(page.getByLabel("Community name")).toBeFocused();
        await expectPage(page, "/games/league-of-legends/post/community");

        await page.getByLabel("Community name").fill("Rift Academy");
        await expect(page.getByText("Give your community a name.")).toHaveCount(0);
    });

    test("shows the account's facts as they are, and says a change reaches every post", async ({ page }) => {
        await signUp(page);
        await page.goto("/games/league-of-legends/post/player");
        await page.getByLabel("Discord", { exact: true }).fill("rift.owl");
        await page.getByRole("button", { name: "Publish post" }).click();
        await expectPage(page, "/games/league-of-legends/post/player/live");

        await page.goto("/games/league-of-legends/post/group");
        const discord = field(page, "Discord");
        await expect(discord.locator(".account-fact")).toContainText("rift.owl");
        await expect(discord.locator(".account-fact")).toContainText("From your account");
        await expect(field(page, "Timezone").locator(".account-fact")).toBeVisible();

        await discord.getByRole("button", { name: "Change" }).click();
        await expect(page.getByLabel("Discord", { exact: true })).toHaveValue("rift.owl");
        await page.getByLabel("Discord", { exact: true }).fill("rift.hawk");
        await expect(discord).toContainText("Applies to all your posts");
    });

    test("publishes a draft written signed out once the player has signed up", async ({ page }) => {
        await page.goto("/games/league-of-legends/post/player");
        await expect(page.getByText("You'll create an account next. Nothing you've written is lost.")).toBeVisible();
        await expect(page.getByRole("complementary", { name: "Preview" }).locator(".card-name")).toHaveText("You");
        await page.getByLabel("About you and what you're looking for").fill("Support main, evenings.");

        await page.getByRole("button", { name: "Publish post" }).click();
        await expectPage(page, "/signup");
        await expect(page.getByText("Your League of Legends player post goes live as soon as you're signed up.")).toBeVisible();
        const nickname = unique("P");
        await page.getByLabel("Email").fill(`${nickname.toLowerCase()}@example.com`);
        await page.getByLabel("Nickname").fill(nickname);
        await page.getByLabel("Password").fill(password);
        await page.getByRole("button", { name: "Create account and publish" }).click();

        await expectPage(page, "/games/league-of-legends/post/player/live");
        await expect(page.getByRole("heading", { name: "Your post is live" })).toBeVisible();

        // The post answers none of the feed's fields, so the feed it describes is the whole
        // feed, the post in it.
        await page.goto(feedPath);
        await expectSettled(page);
        await expect(card(page, nickname)).toContainText("Support main, evenings.");
    });

    test("signs up with Discord beside the Discord input and comes back to the draft", async ({ page }) => {
        const discord = await fakeDiscord(page, discordUser(`${unique("dpost")}@example.com`, true));
        await page.goto("/games/league-of-legends/post/player");
        await page.getByLabel("About you and what you're looking for").fill("Jungle, weekends.");

        await page.getByRole("button", { name: "Sign up with Discord" }).click();
        await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
        await page.getByLabel("Nickname").fill(unique("D"));
        await page.getByRole("button", { name: "Continue" }).click();

        await expectPage(page, "/games/league-of-legends/post/player");
        await expect(page.getByLabel("About you and what you're looking for")).toHaveValue("Jungle, weekends.");
        await expect(field(page, "Discord").locator(".account-fact")).toContainText(discord.user.username);
        await expect(page.getByText("You'll create an account next.")).toHaveCount(0);

        await page.getByRole("button", { name: "Publish post" }).click();
        await expectPage(page, "/games/league-of-legends/post/player/live");
    });

    test("publishes once a player new to Discord's sign-up has picked a nickname", async ({ page }) => {
        await fakeDiscord(page, discordUser(null, false));
        await page.goto("/games/league-of-legends/post/player");
        await page.getByLabel("About you and what you're looking for").fill("Mid, most nights.");

        await page.getByRole("button", { name: "Publish post" }).click();
        await expectPage(page, "/signup");
        await page.getByRole("button", { name: "Continue with Discord" }).click();
        await expect(page.getByRole("heading", { name: "Pick a nickname" })).toBeVisible();
        await page.getByLabel("Nickname").fill(unique("D"));
        await page.getByRole("button", { name: "Publish post" }).click();

        await expectPage(page, "/games/league-of-legends/post/player/live");
        await expect(page.getByRole("heading", { name: "Your post is live" })).toBeVisible();
    });

    test("signing in to publish offers to update the post the player already has", async ({ page }) => {
        const nickname = await signUp(page);
        const name = unique("Rift Owls ");
        await publishGroup(page, name);
        await signOut(page);

        await page.goto("/games/league-of-legends/post/group");
        const renamed = unique("Rift Hawks ");
        await page.getByLabel("Group name").fill(renamed);
        await page.getByRole("button", { name: "Publish post" }).click();
        await expectPage(page, "/signup");
        await page.locator(".flow").getByRole("link", { name: "Sign in" }).click();
        await expectPage(page, "/signin");
        await expect(page.getByRole("heading", { name: "Sign in to publish" })).toBeVisible();
        await page.getByLabel("Email or nickname").fill(nickname);
        await page.getByLabel("Password").fill(password);
        await page.getByRole("button", { name: "Sign in and publish" }).click();

        await expectPage(page, "/games/league-of-legends/post/group");
        await expect(page.getByRole("heading", { name: "You already have a League of Legends group post" })).toBeVisible();
        await expect(card(page, name)).toHaveCount(1);
        await expect(card(page, renamed)).toHaveCount(1);
        await page.getByRole("button", { name: "Update my post" }).click();

        await expectPage(page, "/games/league-of-legends/post/group/live");
        await expect(page.getByRole("heading", { name: "Your post is updated" })).toBeVisible();
        await page.goto("/games/league-of-legends/post/group");
        await expect(card(page, renamed)).toHaveCount(1);
        await expect(card(page, name)).toHaveCount(0);
    });

    test("signing in with Discord can keep the post the player already has", async ({ page }) => {
        const discord = await fakeDiscord(page, discordUser(`${unique("dkeep")}@example.com`, true));
        await signUpWithDiscord(page, discord);
        const name = unique("Rift Owls ");
        await publishGroup(page, name);
        await signOut(page);

        await page.goto("/games/league-of-legends/post/group");
        const renamed = unique("Rift Hawks ");
        await page.getByLabel("Group name").fill(renamed);
        await page.getByRole("button", { name: "Sign up with Discord" }).click();

        await expectPage(page, "/games/league-of-legends/post/group");
        await expect(page.getByRole("heading", { name: "You already have a League of Legends group post" })).toBeVisible();
        await page.getByRole("button", { name: "Keep my post as it is" }).click();
        await expectPage(page, feedPath);

        await page.goto("/games/league-of-legends/post/group");
        await expect(page.getByRole("button", { name: "Edit it" })).toBeVisible();
        await expect(card(page, name)).toHaveCount(1);
        await expect(card(page, renamed)).toHaveCount(0);
    });

    test("starts from the type, then the game, marking the games the player has posted in", async ({ page }) => {
        await signUp(page);
        await publishGroup(page, unique("Rift Owls "));

        await page.goto("/post");
        await page.getByRole("link", { name: /^We're a group looking for players/ }).click();
        await expectPage(page, "/post/group");
        await expect(page.getByRole("link", { name: /^League of Legends/ })).toContainText("Your post");
        await expect(page.locator(".cover-mark")).toHaveCount(1);

        await page.getByRole("link", { name: /^Valorant/ }).click();
        await expectPage(page, "/games/valorant/post/group");
        await expect(page.getByRole("heading", { name: "Tell players about your group" })).toBeVisible();
    });
});
