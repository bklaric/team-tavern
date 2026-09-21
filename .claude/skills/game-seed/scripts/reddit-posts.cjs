// Saves a subreddit's newest posts as JSON, for counting how players of a game
// state roles and what they're after.
//
//   "$(volta which node)" .claude/skills/game-seed/scripts/reddit-posts.cjs <outdir> <max> <subreddit>...
//
// Writes <outdir>/<subreddit>.json: an array of { id, title, flair, text,
// created }. Reddit refuses scripted requests and its JSON listings, but
// serves a browser: the script opens the subreddit in Playwright's Chromium,
// then pages the endpoint the feed's own infinite scroll calls, from inside the
// page so the session cookies go along. It stops at <max>, at the end of the
// feed, or when Reddit starts refusing (a 429 is printed), keeping what it got.

const { chromium } = require("playwright");
const fs = require("fs");
const path = require("path");

const main = async () => {
    const [outdir, max, ...subreddits] = process.argv.slice(2);
    if (!outdir || !max || subreddits.length === 0) {
        console.error("usage: reddit-posts.cjs <outdir> <max> <subreddit>...");
        process.exit(2);
    }
    fs.mkdirSync(outdir, { recursive: true });
    const browser = await chromium.launch();
    const page = await browser.newPage({
        userAgent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    });
    for (const subreddit of subreddits) {
        await page.goto(`https://www.reddit.com/r/${subreddit}/new/`, { waitUntil: "domcontentloaded" });
        await page.waitForTimeout(2000);
        const { posts, refused } = await page.evaluate(async ({ subreddit, max }) => {
            const read = doc => [...doc.querySelectorAll("shreddit-post")].map(post => ({
                id: post.getAttribute("id"),
                title: post.getAttribute("post-title"),
                flair: ((post.querySelector("shreddit-post-flair") || {}).textContent || "").trim().replace(/\s+/g, " "),
                text: ((post.querySelector('[slot="text-body"]') || {}).textContent || "").trim(),
                created: (post.getAttribute("created-timestamp") || "").slice(0, 10),
            }));
            const seen = new Map(read(document).map(post => [post.id, post]));
            let refused = "";
            while (seen.size > 0 && seen.size < max) {
                const last = [...seen.values()].pop();
                const response = await fetch(`/svc/shreddit/community-more-posts/new/?after=${encodeURIComponent(btoa(last.id))}&t=DAY&name=${subreddit}&feedLength=${seen.size}`);
                if (!response.ok) {
                    refused = String(response.status);
                    break;
                }
                const before = seen.size;
                read(new DOMParser().parseFromString(await response.text(), "text/html"))
                    .forEach(post => seen.set(post.id, post));
                if (seen.size === before) break;
                await new Promise(resolve => setTimeout(resolve, 1200));
            }
            return { posts: [...seen.values()], refused };
        }, { subreddit, max: Number(max) });
        fs.writeFileSync(path.join(outdir, `${subreddit}.json`), JSON.stringify(posts));
        const dates = posts.map(post => post.created).filter(Boolean).sort();
        console.log(`${subreddit}: ${posts.length} posts, ${dates[0] || "-"} to ${dates[dates.length - 1] || "-"}${refused ? `, refused with ${refused}` : ""}`);
    }
    await browser.close();
};

main();
