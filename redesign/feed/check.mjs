// Checks feed.sql's marks against the prototype's compare() on the same posts:
//   "$(volta which node)" redesign/feed/check.mjs
// The prototype runs on the samples export-sample.sh writes, so export the
// games below first. The query runs with its batch limits lifted, so every post
// in the game is marked, and each sampled post's marks, misses and compared
// count have to agree.
import { chromium } from "playwright";
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "../..");
const NOW = "2026-09-12T08:23:58Z";
const TZ = "Europe/Zagreb";

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
const query = readFileSync(join(root, "redesign/feed/feed.sql"), "utf8")
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

const browser = await chromium.launch();
let failures = 0;
for (const c of cases) {
    const page = await browser.newPage({ timezoneId: TZ });
    page.on("pageerror", e => console.error(e.message));
    await page.goto(pathToFileURL(join(root, "redesign/prototype/feed.html")).href + `?game=${c.game}`, { waitUntil: "load" });
    const proto = await page.evaluate(({ type, types, d }) =>
        POSTS.filter(p => /^\d+$/.test(p.id) && types.includes(p.type)).map(p => {
            const m = compare(p, type, d);
            const marks = Object.fromEntries(Object.entries(m)
                .filter(([k]) => !["compared", "fits", "misses"].includes(k))
                .map(([k, v]) => [k.replace(/^(field|range):/, ""), v]));
            return { id: p.id, marks, misses: m.misses, compared: m.compared };
        }), { type: c.type, types: c.types, d: c.proto });
    await page.close();
    const sql = sqlMarks(c);
    let differ = 0;
    for (const p of proto) {
        const s = sql.get(p.id);
        const same = s && JSON.stringify(Object.entries(s.marks).sort()) === JSON.stringify(Object.entries(p.marks).sort())
            && s.misses === p.misses && s.compared === p.compared;
        if (!same) {
            if (differ < 5) console.log(`  ${c.name} #${p.id}\n    prototype ${JSON.stringify(p.marks)}\n    query     ${JSON.stringify(s && s.marks)}`);
            differ++;
        }
    }
    console.log(`${c.name}: ${proto.length} posts compared, ${differ} differ (query returned ${sql.size})`);
    failures += differ;
}
await browser.close();
process.exit(failures ? 1 : 0);
