// Puts a SteamID64 in place of every Steam contact that isn't one, as the
// server now requires. A contact or link with one ID in it gives that ID; the
// name a link gives is asked of the Steam Web API as a custom address, then
// read as a friend code; a contact that names no profile is cleared.
//
// It runs in the node container, whose environment has the database's
// variables and STEAM_API_KEY, and whose node_modules has pg. Without --apply
// it only prints what it would do:
//
//     ssh user@host "docker exec -i -w /root/team-tavern/server tt-node node -" \
//         < src/TeamTavern/Database/Migrations/2026-10-06-steam-id.js
//     ssh user@host "docker exec -i -w /root/team-tavern/server tt-node node - --apply" \
//         < src/TeamTavern/Database/Migrations/2026-10-06-steam-id.js
//
// It writes nothing unless Steam answers every lookup, writes everything in
// one transaction, and leaves alone a contact its owner changed after it was
// read. Run again, it finds nothing left to do.

const { Client } = require("pg");

const apply = process.argv.includes("--apply");
const steamApiUrl = process.env.STEAM_API_URL ?? "https://api.steampowered.com";
const steamApiKey = process.env.STEAM_API_KEY;

const steamIdPattern = /^7656119\d{10}$/;
const anySteamId = /(?<!\d)7656119\d{10}(?!\d)/g;
// A link to steamcommunity.com, misspelt or not, and what follows its host,
// past /id/, /profiles/ or /profile/ if it has one.
const steamLink = /^(?:https?:\/\/)?(?:www\.)?steamcomm?unity\.com\/(?:(id|profiles?)\/)?([^/?#]*)/i;
// Pages of Steam's own a link can lead to that are no player's.
const steamPages = ["home", "groups", "id", "profiles", "profile", "market", "app", "workshop", "sharedfiles", "my"];
const vanityPattern = /^[A-Za-z0-9_.-]{2,32}$/;
// Steam's Add a Friend shows a friend code: the 32-bit account ID that the
// SteamID64 of an individual account counts up from. A player's has eight
// digits or more.
const accountIdBase = 76561197960265728n;

function fromFriendCode(digits) {
    if (!/^\d{6,10}$/.test(digits)) return null;
    const accountId = BigInt(digits);
    return accountId > 0n && accountId < 2n ** 32n ? String(accountIdBase + accountId) : null;
}

async function resolveVanity(vanity) {
    const url = `${steamApiUrl}/ISteamUser/ResolveVanityURL/v1/`
        + `?key=${encodeURIComponent(steamApiKey)}&vanityurl=${encodeURIComponent(vanity)}`;
    const response = await fetch(url, { signal: AbortSignal.timeout(10000) });
    const body = await response.text();
    if (response.status !== 200) {
        throw new Error(`Steam answered ${response.status} for ${vanity}: ${body}`);
    }
    const { success, steamid } = JSON.parse(body).response;
    if (success === 1 && steamIdPattern.test(steamid)) return steamid;
    if (success === 42) return null;
    throw new Error(`Steam answered ${body} for ${vanity}`);
}

// The SteamID64 a contact names, or null, and how it was read. A link's last
// resort is a friend code, after Steam has no custom address of that name.
async function read(contact) {
    const text = contact.trim();
    if (steamIdPattern.test(text)) return { steamId: text, how: "ID" };
    const ids = [...new Set(text.match(anySteamId) ?? [])];
    if (ids.length === 1) return { steamId: ids[0], how: "ID in a link" };
    if (fromFriendCode(text)) return { steamId: fromFriendCode(text), how: "friend code" };
    const link = text.match(steamLink);
    if (!link) return { steamId: null, how: "no link" };
    let segment = link[2];
    try { segment = decodeURIComponent(segment); } catch {}
    segment = segment.trim();
    if (link[1] === undefined && steamPages.includes(segment.toLowerCase())) {
        return { steamId: null, how: "link to no profile" };
    }
    if (vanityPattern.test(segment)) {
        const steamId = await resolveVanity(segment);
        if (steamId) return { steamId, how: "custom address" };
    }
    const friendCode = fromFriendCode(segment);
    if (friendCode) return { steamId: friendCode, how: "friend code in a link" };
    return { steamId: null, how: "custom address no profile has" };
}

async function main() {
    if (!steamApiKey) throw new Error("STEAM_API_KEY isn't set.");
    const client = new Client();
    await client.connect();
    try {
        const { rows } = await client.query(`
            select id, nickname, steam_id
            from player
            where steam_id is not null and steam_id !~ '^7656119[0-9]{10}$'
            order by id`);
        const changes = [];
        for (const row of rows) {
            changes.push({ ...row, ...await read(row.steam_id) });
        }

        for (const { id, nickname, steam_id, steamId, how } of changes) {
            console.log([id, nickname, JSON.stringify(steam_id), "->", steamId ?? "cleared", `(${how})`].join("\t"));
        }
        const counts = {};
        for (const { how } of changes) counts[how] = (counts[how] ?? 0) + 1;
        console.log(`\n${changes.length} contacts:`, counts);

        // A post that asks to be added off-site keeps asking once the Steam
        // contact goes, and offers nothing but messages if that was the only
        // way its game's contacts, Discord server or website gave. The columns
        // are those ContactAccount.purs reads for each kind.
        const cleared = changes.filter(change => change.steamId === null).map(change => change.id);
        const { rows: stranded } = await client.query(`
            select post.id, game.handle, post.ilk, player.nickname
            from post
            join game on game.id = post.game_id
            join player on player.id = post.player_id
            where post.player_id = any($1::int[])
                and post.contact_preference = 'offsite'
                and post.discord_server is null
                and post.website is null
                and exists (
                    select 1 from game_contact
                    where game_contact.game_id = post.game_id and game_contact.kind = 'steam')
                and not exists (
                    select 1 from game_contact
                    where game_contact.game_id = post.game_id
                        and case game_contact.kind
                            when 'discord' then player.discord_tag
                            when 'riot' then player.riot_id
                            when 'battle_tag' then player.battle_tag
                            when 'ea' then player.ea_id
                            when 'epic' then player.epic_id
                            when 'embark' then player.embark_id
                            when 'ubisoft' then player.ubisoft_username
                            when 'marvel_rivals' then player.marvel_rivals_username
                            when 'psn' then player.psn_id
                            when 'gamer_tag' then player.gamer_tag
                            when 'friend_code' then player.friend_code
                        end is not null)
            order by post.id`,
            [cleared]);
        console.log(`\n${stranded.length} off-site posts are left with no contact but messages:`);
        for (const { id, handle, ilk, nickname } of stranded) {
            console.log([id, handle, ilk, nickname].join("\t"));
        }

        if (!apply) {
            console.log("\nNothing written. Run with --apply to write it.");
            return;
        }
        await client.query("begin");
        let written = 0;
        for (const { id, steam_id, steamId } of changes) {
            const result = await client.query(
                "update player set steam_id = $1 where id = $2 and steam_id = $3",
                [steamId, id, steam_id]);
            written += result.rowCount;
        }
        await client.query("commit");
        console.log(`\nWrote ${written} of ${changes.length} contacts.`);
    } catch (error) {
        await client.query("rollback").catch(() => {});
        throw error;
    } finally {
        await client.end();
    }
}

main().catch(error => {
    console.error(error.message);
    process.exitCode = 1;
});
