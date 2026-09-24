// Screenshots prototype pages at phone and desktop width:
//   node redesign/prototype/screenshot.mjs [--sections] [--parts] [--locale=en-GB] [page.html[?query] ...]
// With no pages it takes every .html file. A very tall page is hard to review
// whole: --sections also captures each top-level <section> on its own, and
// --parts cuts the page into slices. Errors the page throws or logs are printed.
import { chromium } from "playwright";
import { mkdirSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const sections = args.includes("--sections");
const parts = args.includes("--parts");
const PART = 1400;
// --locale=en-GB and the like, for what depends on the viewer's locale.
const locale = (args.find(arg => arg.startsWith("--locale=")) || "").slice("--locale=".length) || undefined;
const named = args.filter(arg => arg.includes(".html"));
const pages = named.length ? named : readdirSync(here).filter(name => name.endsWith(".html"));
// The site's test-playwright/screenshots.mjs takes the same widths, so the two compare.
const widths = { phone: 375, desktop: 1280 };

const slug = page => page
    .replace(".html", "")
    .replace(/describe=[^&]*/, match => `described-${match.length}`)
    .replace(/[^a-z0-9-]+/gi, "-")
    .replace(/-+$/, "");

mkdirSync(join(here, "screenshots"), { recursive: true });
const browser = await chromium.launch();
for (const page of pages) {
    const [path, query] = page.split("?");
    const url = pathToFileURL(join(here, path)).href + (query ? `?${query}` : "");
    for (const [device, width] of Object.entries(widths)) {
        const tab = await browser.newPage({ viewport: { width, height: 900 }, locale });
        tab.on("pageerror", error => console.error(`${page} ${device}: ${error.message}`));
        tab.on("console", message => message.type() === "error" && console.error(`${page} ${device}: ${message.text()}`));
        await tab.goto(url, { waitUntil: "networkidle" });
        await tab.evaluate(() => document.fonts.ready);
        const file = join(here, "screenshots", `${slug(page)}-${device}.png`);
        await tab.screenshot({ path: file, fullPage: true });
        console.log(file);
        if (parts) {
            const height = await tab.evaluate(() => document.documentElement.scrollHeight);
            for (let y = 0, i = 1; y < height; y += PART, i++) {
                const part = join(here, "screenshots", `${slug(page)}-${device}-part${i}.png`);
                await tab.screenshot({ path: part, fullPage: true, clip: { x: 0, y, width, height: Math.min(PART, height - y) } });
                console.log(part);
            }
        }
        if (sections) {
            const found = await tab.locator("main > section").all();
            for (const [i, section] of found.entries()) {
                const part = join(here, "screenshots", `${slug(page)}-${device}-${i + 1}.png`);
                await section.screenshot({ path: part });
                console.log(part);
            }
        }
        await tab.close();
    }
}
await browser.close();
