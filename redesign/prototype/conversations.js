// Handwritten conversations the prototype's store starts with. Production has
// no messages, and its players' words stay out of the repo, so the people here
// are made up, apart from the prototype's own accounts: Kestrel, who owns Night
// Owls and a Dota 2 post, and Vex.
//
// Each post is kept as the conversation saw it: the facts its card showed, its
// contacts, and how its owner wants to be reached. A starter's post is the one
// they have in the same game, if any, which the owner sees in the header.

const NIGHT_OWLS_POST = {
    id: "kestrel-valorant-group",
    game: "valorant",
    type: "group",
    name: "Night Owls",
    owner: "Kestrel",
    updated: "2026-09-06T19:12:00Z",
    slots: { members: 3, total: 5 },
    facts: [
        { text: "Platinum – Diamond" },
        { text: "Needs Lurker, Supporter" },
        { text: "EU" },
        { text: "EN, HR" },
        { icon: "mic", label: "Microphone required" },
        { text: "Ages 18+" },
        { text: "Competitive" },
    ],
    reach: "message",
    contacts: [
        { label: "Discord", value: "kestrel" },
        { label: "Riot ID", value: "Kestrel#EUW" },
    ],
};

const KESTREL_DOTA_POST = {
    id: "kestrel-dota2-player",
    game: "dota2",
    type: "player",
    name: "Kestrel",
    owner: "Kestrel",
    updated: "2026-07-20T17:40:00Z",
    facts: [{ text: "Legend 2" }, { text: "Soft support, Hard support" }, { text: "Croatia" }, { text: "HR, EN" }],
    reach: "offsite",
    contacts: [
        { label: "Discord", value: "kestrel" },
        { label: "Steam profile", value: "steamcommunity.com/id/kestrel", url: "https://steamcommunity.com/id/kestrel" },
    ],
};

const CONVERSATION_FIXTURES = [
    {
        id: "c-vex-night-owls",
        post: NIGHT_OWLS_POST,
        starter: { nickname: "Vex", post: null },
        messages: [
            { from: "Vex", at: "2026-09-10T18:14:00Z", text: "Hey, saw you need a support. I main Sage and Skye, Plat 3, usually on from 9. Still looking?" },
            { from: "Kestrel", at: "2026-09-10T18:40:00Z", text: "Hey! Yes, still two spots. We play around 21:00 most nights, want to join us tonight?" },
            { from: "Vex", at: "2026-09-10T18:42:00Z", text: "Sure, add me: vex.omen" },
            { from: "Kestrel", at: "2026-09-10T18:45:00Z", text: "Sent. See you at 9 👋" },
        ],
        read: { Vex: 4, Kestrel: 4 },
    },
    {
        id: "c-ashen-night-owls",
        post: NIGHT_OWLS_POST,
        starter: {
            nickname: "Ashen",
            post: {
                type: "player",
                name: "Ashen",
                facts: [{ text: "Diamond" }, { text: "Supporter, Lurker" }, { text: "Germany" }, { text: "EN, DE" }, { icon: "mic", label: "Microphone" }],
            },
        },
        messages: [
            { from: "Ashen", at: "2026-09-11T22:48:00Z", text: "Hi, Diamond support here, mostly Killjoy and Cypher. Free most nights after 21:00.\n\nDo you still have room? Happy to play a few unrated games first to see if we click." },
        ],
        read: {},
    },
    {
        id: "c-danya-night-owls",
        post: NIGHT_OWLS_POST,
        starter: { nickname: "Danya", post: null },
        messages: [
            { from: "Danya", at: "2026-09-08T17:05:00Z", text: "Привет! Играю за лёркера, ранг Платина. Английский понимаю, но говорю лучше по-русски. Можно к вам?" },
            { from: "Kestrel", at: "2026-09-08T19:31:00Z", text: "Hi! We talk in English on comms. Is that OK for you?" },
            { from: "Danya", at: "2026-09-08T19:50:00Z", text: "Yes ok, I try :) Add me in game: Danya#RU1" },
        ],
        read: { Danya: 3, Kestrel: 3 },
    },
    {
        id: "c-tidebringer-kestrel-dota",
        post: KESTREL_DOTA_POST,
        starter: {
            nickname: "Tidebringer",
            post: {
                type: "group",
                name: "Ancient Echoes",
                facts: [{ text: "Legend – Ancient" }, { text: "Needs Hard support" }, { text: "EU" }, { text: "EN" }],
            },
        },
        messages: [
            { from: "Tidebringer", at: "2026-09-03T16:20:00Z", text: "We're a four-stack around Legend looking for a pos 5, saw your post. Want to try a few games this weekend?" },
            { from: "Kestrel", at: "2026-09-04T08:02:00Z", text: "Maybe! Which days?" },
            { from: "Tidebringer", at: "2026-09-04T09:15:00Z", text: "Saturday and Sunday evening, from about 20:00." },
        ],
        read: { Tidebringer: 3, Kestrel: 3 },
    },
    {
        id: "c-kestrel-farlands",
        post: {
            id: "eirik-valheim-community",
            game: "valheim",
            type: "community",
            name: "The Farlands",
            owner: "Eirik",
            updated: "2026-09-01T12:00:00Z",
            facts: [{ text: "Dedicated server" }, { text: "EU" }, { text: "EN" }, { text: "Events, Casual" }, { text: "All experience levels" }],
            reach: "discord",
            contacts: [
                { label: "Discord invite", value: "discord.gg/farlands", url: "https://discord.gg/farlands" },
                { label: "Website", value: "thefarlands.gg", url: "https://thefarlands.gg" },
            ],
        },
        starter: { nickname: "Kestrel", post: null },
        messages: [
            { from: "Kestrel", at: "2026-09-07T20:10:00Z", text: "Hi! A friend and I just started over in Valheim. Is the server still open to new players?" },
            { from: "Eirik", at: "2026-09-12T07:41:00Z", text: "Welcome! Yes, join the Discord and grab the Viking role in #roles, then ping me and I'll add you both to the whitelist." },
        ],
        read: { Kestrel: 1, Eirik: 2 },
    },
    {
        id: "c-kestrel-shadowfox",
        post: {
            id: "shadowfox-valorant-player",
            game: "valorant",
            type: "player",
            name: "ShadowFox",
            owner: "ShadowFox",
            updated: "2026-09-10T09:00:00Z",
            facts: [{ text: "Diamond" }, { text: "Entry fragger, Sniper" }, { text: "Croatia" }, { text: "EN, DE" }, { icon: "mic", label: "Microphone" }, { text: "Competitive" }],
            reach: "offsite",
            contacts: [
                { label: "Discord", value: "shadowfox" },
                { label: "Riot ID", value: "ShadowFox#EUW" },
            ],
        },
        starter: { nickname: "Kestrel", post: null },
        messages: [
            { from: "Kestrel", at: "2026-09-11T18:30:00Z", text: "Want to duo tonight? Diamond support, on from 21:00." },
        ],
        read: { Kestrel: 1 },
    },
    {
        id: "c-kestrel-quill",
        post: {
            id: "quill-valorant-player",
            game: "valorant",
            type: "player",
            name: "Quill",
            owner: "Quill",
            updated: "2026-05-02T10:00:00Z",
            facts: [{ text: "Gold" }, { text: "Supporter" }, { text: "Slovenia" }, { text: "SL, EN" }],
            reach: "message",
            contacts: [],
        },
        starter: { nickname: "Kestrel", post: null },
        messages: [
            { from: "Kestrel", at: "2026-08-20T19:00:00Z", text: "Hey, are you still looking for people to play with?" },
        ],
        read: { Kestrel: 1 },
    },
];
