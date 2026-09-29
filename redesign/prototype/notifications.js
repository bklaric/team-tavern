// Handwritten notifications the prototype's store starts with (brief 8, 11.3).
// Each sits under one of the player's own posts: a post that fits it, or the
// post's own expiry. The posts that fit are real rows from the dump, so a row
// opens the page it names; Dota 2 has no sample, so its two are made up, as the
// conversations are, and reach the same "No sample" page the rest of Kestrel's
// Dota 2 post does.
//
// A notification is stamped when the post that fits it was published, which is
// what its page says it was, and the expiry ones when the renewal email went
// out, a week before the post expires. Kestrel's posts are older than the last
// renewal their cards count from, so the rows under them predate it.

const NOTIFICATION_FIXTURES = [
    // Night Owls, active until 6 October: Valorant players who fit it.
    {
        id: "n-night-owls-14487",
        for: "Kestrel",
        post: { id: "kestrel-valorant-group", game: "valorant", type: "group", name: "Night Owls" },
        kind: "fits",
        about: { id: "14487", game: "valorant", type: "player", name: "NightHell" },
        at: "2026-08-13T12:55:23Z",
        read: false,
    },
    {
        // Since expired: the row still opens the post's page, which says so.
        id: "n-night-owls-25532",
        for: "Kestrel",
        post: { id: "kestrel-valorant-group", game: "valorant", type: "group", name: "Night Owls" },
        kind: "fits",
        about: { id: "25532", game: "valorant", type: "player", name: "purpiii" },
        at: "2026-07-25T22:24:33Z",
        read: false,
    },
    {
        id: "n-night-owls-26377",
        for: "Kestrel",
        post: { id: "kestrel-valorant-group", game: "valorant", type: "group", name: "Night Owls" },
        kind: "fits",
        about: { id: "26377", game: "valorant", type: "player", name: "Tatami" },
        at: "2026-07-24T14:33:09Z",
        read: true,
    },
    // Kestrel's Valheim post, in its last week.
    {
        id: "n-valheim-expiry",
        for: "Kestrel",
        post: { id: "kestrel-valheim-player", game: "valheim", type: "player", name: "Kestrel" },
        kind: "expiry",
        at: "2026-09-08T10:00:00Z",
        read: false,
    },
    // Kestrel's Dota 2 post, expired on 19 August. What fits it came while it
    // was still active; nothing has since, and nothing will until it is renewed.
    {
        id: "n-dota2-expiry",
        for: "Kestrel",
        post: { id: "kestrel-dota2-player", game: "dota2", type: "player", name: "Kestrel" },
        kind: "expiry",
        at: "2026-08-12T17:40:00Z",
        read: true,
    },
    {
        id: "n-dota2-ancient-echoes",
        for: "Kestrel",
        post: { id: "kestrel-dota2-player", game: "dota2", type: "player", name: "Kestrel" },
        kind: "fits",
        about: { id: "tidebringer-dota2-group", game: "dota2", type: "group", name: "Ancient Echoes" },
        at: "2026-08-15T19:04:00Z",
        read: true,
    },
    {
        id: "n-dota2-ancients",
        for: "Kestrel",
        post: { id: "kestrel-dota2-player", game: "dota2", type: "player", name: "Kestrel" },
        kind: "fits",
        about: { id: "ancients-dota2-community", game: "dota2", type: "community", name: "The Ancients" },
        at: "2026-07-28T12:30:00Z",
        read: true,
    },
];
