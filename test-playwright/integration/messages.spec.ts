import { expect, Page, test } from "@playwright/test";
import { signUp } from "../accounts";

// A player new to the run writes to HeroesOfTheStormTester (`stacks/test-seed/players.sql`),
// whose post no other spec messages, so its conversations are only this spec's, one per
// writer.

const handle = "heroes-of-the-storm";
const owner = "HeroesOfTheStormTester";

const card = (page: Page, name: string) =>
    page.locator(".card").filter({ has: page.getByRole("link", { name, exact: true }) });

async function openFeed(page: Page) {
    await page.goto(`/games/${handle}`);
    await expect(page.locator(".feed")).toHaveAttribute("aria-busy", "false");
}

test.describe("messaging", () => {
    test("starts a conversation from the contact panel, which the card then says", async ({ page }) => {
        await signUp(page, "W");
        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });
        await expect(panel.getByRole("heading", { name: `Message ${owner}` })).toBeVisible();

        const box = panel.getByRole("textbox", { name: "Message" });
        await box.fill("Hi, want to play tonight?");
        await box.press("Shift+Enter");
        await box.pressSequentially("I'm on from nine.");
        await box.press("Enter");

        await expect(panel.locator(".message-own")).toHaveText("You: Hi, want to play tonight?\nI'm on from nine.");
        await expect(box).toHaveValue("");
        await expect(panel.getByRole("heading", { name: `Your conversation with ${owner}` })).toBeVisible();
        await page.keyboard.press("Escape");
        await expect(card(page, owner).locator(".card-messaged")).toHaveText("You messaged just now");
        await expect(card(page, owner).locator(".card-contact")).toHaveText("Open conversation");

        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        await expect(panel.locator(".message-own")).toHaveText(["You: Hi, want to play tonight?\nI'm on from nine."]);
        await expect(panel.locator(".thread-new")).toHaveCount(0);
    });

    test("sends nothing that is only blank", async ({ page }) => {
        await signUp(page, "W");
        await openFeed(page);
        await card(page, owner).locator(".card-contact").click();
        const panel = page.getByRole("dialog", { name: owner });

        await panel.getByRole("textbox", { name: "Message" }).fill("   ");
        await panel.getByRole("button", { name: "Send" }).click();

        await expect(panel.locator(".message")).toHaveCount(0);
        await expect(panel.getByText("Your message starts a conversation about HeroesOfTheStormTester's post."))
            .toBeVisible();
    });
});
