// Checks the feed query's marks against the prototype's compare() on the same posts:
//   "$(volta which node)" redesign/feed/check.mjs
//   "$(volta which node)" redesign/feed/check.mjs --api http://localhost:8090
// The prototype runs on the samples export-sample.sh writes, so export the
// games below first. Without --api the query runs through psql with its batch
// limits lifted, so every post in the game is marked. With --api the feed
// endpoint of a server on redesign_import answers instead, batch after batch
// along its cursor, and the prototype judges at the server's time rather than
// the samples' so timezones agree. The samples carry ages worked out at the
// dump's date, which the server's date can move, so this way leaves the age
// marks out and the psql way checks them. Each sampled post's marks, misses and
// compared count have to agree.
import { chromium } from "playwright";
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";
import { dirname, extname, join, normalize } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "../..");
const feedSql = join(root, "src/TeamTavern/Server/Feed/Feed.sql");
const prototype = join(root, "redesign/prototype");
const NOW = "2026-09-12T08:23:58Z";
const TZ = "Europe/Zagreb";
const api = process.argv.includes("--api") ? process.argv[process.argv.indexOf("--api") + 1] : null;

// Each case: the prototype's description and the query's.
const cases = [
    {
        name: "player, every field", game: "valorant", type: "player", types: ["player", "group", "community"],
        proto: {
            "field:rank": "diamond-2", "field:role": ["controller", "sentinel"], "field:platform": ["pc"],
            "field:looking-for": ["ranked"], "field:in-game-leader": true,
            location: "Croatia", age: "24", languages: ["English"], hours: { from: "19:00", to: "23:00" }, mic: true,
        },
        sql: {
            type: "player", options: { rank: ["diamond-2"], role: ["controller", "sentinel"], platform: ["pc"], "looking-for": ["ranked"] },
            flags: ["in-game-leader"], country: "Croatia", age: 24, languages: ["English"],
            online: { from: "19:00", to: "23:00" }, timezone: TZ, microphone: true,
        },
    },
    {
        name: "player, single role, late hours", game: "valorant", type: "player", types: ["player", "group", "community"],
        proto: { "field:rank": "gold-1", "field:role": ["duelist"], location: "United States", age: "17", hours: { from: "22:30", to: "02:00" } },
        sql: { type: "player", options: { rank: ["gold-1"], role: ["duelist"] }, country: "United States", age: 17, online: { from: "22:30", to: "02:00" }, timezone: TZ },
    },
    {
        name: "group, every field", game: "valorant", type: "group", types: ["player"],
        proto: {
            "field:role": ["controller", "initiator"], "field:platform": ["pc"], "range:rank": { from: "platinum-1", to: "diamond-3" },
            regions: ["Europe"], ageRange: { from: "18", to: "30" }, languages: ["English", "German"],
            hours: { from: "20:00", to: "02:00" }, mic: true, "field:in-game-leader": true,
        },
        sql: {
            type: "group", options: { role: ["controller", "initiator"], platform: ["pc"] }, ranges: { rank: { from: "platinum-1", to: "diamond-3" } },
            regions: ["Europe"], ageFrom: 18, ageTo: 30, languages: ["English", "German"],
            online: { from: "20:00", to: "02:00" }, timezone: TZ, microphone: true, flags: ["in-game-leader"],
        },
    },
    {
        name: "community, open range", game: "valorant", type: "community", types: ["player"],
        proto: { "range:rank": { from: "ascendant-1" }, regions: ["North America", "Europe"], ageRange: { from: "16" }, "field:looking-for": ["casual"] },
        sql: { type: "community", ranges: { rank: { from: "ascendant-1", to: null } }, regions: ["North America", "Europe"], ageFrom: 16, options: { "looking-for": ["casual"] } },
    },
    {
        name: "lol player", game: "lol", type: "player", types: ["player", "group", "community"],
        proto: { "field:role": ["mid"], location: "Germany", languages: ["English"], hours: { from: "18:00", to: "22:00" }, mic: true },
        sql: { type: "player", options: { role: ["mid"] }, country: "Germany", languages: ["English"], online: { from: "18:00", to: "22:00" }, timezone: TZ, microphone: true },
    },
];

// The whole feed at once: the batch limits lifted.
const query = readFileSync(feedSql, "utf8")
    .replace(/limit 21\b/, "limit 1000000").replace(/limit 20\b/, "limit 1000000")
    .replace(/<= 20\b/, "<= 1000000");
const lit = value => `'${value.replace(/'/g, "''")}'`;
const sqlMarks = c => {
    const sql = `set timezone = 'UTC';
select json_agg(json_build_object('id', id, 'marks', marks, 'misses', misses, 'compared', compared)) from (
${query.replace(/\$1/g, lit(c.game)).replace(/\$2\b/g, "null::integer").replace(/\$3/g, `${lit(JSON.stringify(c.sql))}::jsonb`)
        .replace(/\$4/g, `${lit(`{${c.types.join(",")}}`)}::text[]`).replace(/\$5/g, "null::jsonb").replace(/\$6/g, `${lit(NOW)}::timestamptz`)}
) feed;`;
    const out = execFileSync("docker", ["exec", "-i", "postgres", "psql", "-U", "bklaric", "-d", "redesign_import", "-qAt", "-v", "ON_ERROR_STOP=1"],
        { input: sql, maxBuffer: 1 << 28 }).toString().trim();
    return new Map(JSON.parse(out).map(r => [String(r.id), r]));
};

// The endpoint takes a description with every list and switch given, as the
// client sends it, and gives each card its marks, which are exactly what it
// compared.
const described = d => ({ options: {}, ranges: {}, flags: [], regions: [], languages: [], microphone: false, ...d });
const apiMarks = async c => {
    const marks = new Map();
    let cursor = null;
    do {
        const response = await fetch(`${api}/games/${c.game}/feed`, {
            method: "POST",
            headers: { "content-type": "application/json" },
            body: JSON.stringify({ description: described(c.sql), showing: c.types, cursor }),
        });
        if (!response.ok) throw new Error(`${c.name}: the feed answered ${response.status}`);
        const batch = await response.json();
        for (const post of batch.posts) {
            const values = Object.values(post.marks);
            marks.set(String(post.id), {
                marks: post.marks, compared: values.length, misses: values.filter(v => v !== "fit").length,
            });
        }
        cursor = batch.more ? batch.cursor : null;
    } while (cursor);
    return marks;
};

// The prototype over HTTP, with site.js judging at the time given.
const servePrototype = at => new Promise(resolve => {
    const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".svg": "image/svg+xml" };
    const server = createServer((request, response) => {
        const path = normalize(join(prototype, decodeURIComponent(new URL(request.url, "http://x").pathname)));
        if (!path.startsWith(prototype)) return response.writeHead(403).end();
        let body;
        try { body = readFileSync(path); } catch { return response.writeHead(404).end(); }
        if (path.endsWith("site.js")) {
            body = body.toString().replace(`new Date("${NOW}")`, `new Date("${at}")`);
        }
        response.writeHead(200, { "content-type": types[extname(path)] || "application/octet-stream" }).end(body);
    });
    server.listen(0, "127.0.0.1", () => resolve(server));
});

// A post's marks as the endpoint is checked on: without the ages, and counted
// from what is left.
const comparable = r => {
    if (!api) return r;
    const marks = Object.fromEntries(Object.entries(r.marks).filter(([k]) => k !== "age" && k !== "ages"));
    const values = Object.values(marks);
    return { ...r, marks, compared: values.length, misses: values.filter(v => v !== "fit").length };
};

const at = new Date().toISOString();
const server = api ? await servePrototype(at) : null;
const pageUrl = game => api
    ? `http://127.0.0.1:${server.address().port}/feed.html?game=${game}`
    : pathToFileURL(join(prototype, "feed.html")).href + `?game=${game}`;

const browser = await chromium.launch();
let failures = 0;
for (const c of cases) {
    const page = await browser.newPage({ timezoneId: TZ });
    page.on("pageerror", e => console.error(e.message));
    await page.goto(pageUrl(c.game), { waitUntil: "load" });
    const proto = await page.evaluate(({ type, types, d }) =>
        POSTS.filter(p => /^\d+$/.test(p.id) && types.includes(p.type)).map(p => {
            const m = compare(p, type, d);
            const marks = Object.fromEntries(Object.entries(m)
                .filter(([k]) => !["compared", "fits", "misses"].includes(k))
                .map(([k, v]) => [k.replace(/^(field|range):/, ""), v]));
            return { id: p.id, marks, misses: m.misses, compared: m.compared };
        }), { type: c.type, types: c.types, d: c.proto });
    await page.close();
    const sql = api ? await apiMarks(c) : sqlMarks(c);
    let differ = 0;
    for (const p of proto.map(comparable)) {
        const s = sql.has(p.id) ? comparable(sql.get(p.id)) : undefined;
        const same = s && JSON.stringify(Object.entries(s.marks).sort()) === JSON.stringify(Object.entries(p.marks).sort())
            && s.misses === p.misses && s.compared === p.compared;
        if (!same) {
            if (differ < 5) console.log(`  ${c.name} #${p.id}\n    prototype ${JSON.stringify(p.marks)}\n    ${api ? "endpoint " : "query    "} ${JSON.stringify(s && s.marks)}`);
            differ++;
        }
    }
    console.log(`${c.name}: ${proto.length} posts compared, ${differ} differ (${api ? "endpoint" : "query"} returned ${sql.size})`);
    failures += differ;
}
await browser.close();
server?.close();
process.exit(failures ? 1 : 0);
