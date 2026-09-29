// Checks the fit query against the feed query from every owner's seat:
//   "$(volta which node)" redesign/feed/check-fits.mjs [game ...]
//   "$(volta which node)" redesign/feed/check-fits.mjs --at 2024-09-01
//   "$(volta which node)" redesign/feed/check-fits.mjs --container teamtavern-test-postgres-1 --user tester --database team_tavern_test
// A notification says a post fits another (brief 8), and fitting is what the
// feed's Fits you counts, which check.mjs proves against the prototype. So for
// every post in the games given (all of them by default) the posts Fits.sql
// says it fits have to be the active posts whose own feed, asked with the
// description the post makes (descriptionJson, read from the compiled
// output/, so run spago build first), puts it under Fits you.
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { descriptionJson } from "../../output/TeamTavern.Server.Feed.ViewOwnDescriptions/index.js";

const root = join(dirname(fileURLToPath(import.meta.url)), "../..");
const option = (name, fallback) =>
    process.argv.includes(name) ? process.argv[process.argv.indexOf(name) + 1] : fallback;
const container = option("--container", "postgres");
const user = option("--user", "bklaric");
const database = option("--database", "redesign_import");
const games = process.argv.slice(2).filter((arg, i, args) => !arg.startsWith("--") && !args[i - 1]?.startsWith("--"));
const lit = value => `'${value.replace(/'/g, "''")}'`;
// A dump holds few posts still active today, so --at judges at a time of its
// own: every post updated after the 30 or 90 days before it counts as active.
const at = `${lit(option("--at", new Date().toISOString()))}::timestamptz`;

const fitsSql = readFileSync(join(root, "src/TeamTavern/Server/Feed/Fits.sql"), "utf8");
// The whole feed at once, with the cursor gone and now in its place. An expired
// post is never a fit, so the feed never reaches them.
const feedSql = readFileSync(join(root, "src/TeamTavern/Server/Feed/Feed.sql"), "utf8")
    .replace(/limit 21\b/, "limit 1000000").replace(/limit 20\b/, "limit 1000000")
    .replace(/<= 20\b/, "< 0")
    .replace(/\$5\b/g, "(null::jsonb)").replace(/\$6\b/g, "$5");
const gameFilter = games.length ? `game.handle = any(${lit(`{${games.join(",")}}`)}::text[])` : "true";

const sql = `set timezone = 'UTC';
create function pg_temp.fits(integer, timestamptz) returns setof integer language sql as $fits$
${fitsSql}
$fits$;
create function pg_temp.feed_fits(text, integer, jsonb, text[], timestamptz) returns setof integer
language sql as $feed$
select id from (
${feedSql}
) feed
where not expired and compared > 0 and misses = 0
$feed$;
with seat as (
    select
        post.id,
        post.game_id,
        game.handle,
        post.player_id,
        ${descriptionJson.replace("now()", at)} as description,
        case when post.ilk = 'player' then '{player,group,community}' else '{player}' end::text[] as types
    from post
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    where ${gameFilter}
        and post.updated > ${at} - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end
),
expected as (
    select fitting.id as post, seat.id as seat
    from seat
    cross join lateral pg_temp.feed_fits(seat.handle, seat.player_id, seat.description, seat.types, ${at}) fitting(id)
),
actual as (
    select post.id as post, told.id as seat
    from post
    join game on game.id = post.game_id
    cross join lateral pg_temp.fits(post.id, ${at}) told(id)
    where ${gameFilter}
),
differ as (
    select coalesce(expected.post, actual.post) as post, coalesce(expected.seat, actual.seat) as seat,
        expected.post is not null as feed, actual.post is not null as fits
    from expected
    full join actual on actual.post = expected.post and actual.seat = expected.seat
    where expected.post is null or actual.post is null
)
select json_build_object(
    'posts', (select count(*) from post join game on game.id = post.game_id where ${gameFilter}),
    'pairs', (select count(*) from expected),
    'differ', coalesce((select json_agg(differ) from differ), '[]'))`;

const out = execFileSync("docker",
    ["exec", "-i", container, "psql", "-U", user, "-d", database, "-qAt", "-v", "ON_ERROR_STOP=1"],
    { input: sql, maxBuffer: 1 << 28 }).toString().trim();
const { posts, pairs, differ } = JSON.parse(out);
for (const d of differ.slice(0, 10)) {
    console.log(`  post ${d.post} from post ${d.seat}'s seat: feed ${d.feed ? "fits" : "doesn't"}, Fits.sql ${d.fits ? "fits" : "doesn't"}`);
}
console.log(`${posts} posts, ${pairs} fitting pairs by the feed, ${differ.length} differ`);
process.exit(differ.length ? 1 : 0);
