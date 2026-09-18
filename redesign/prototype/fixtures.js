// Handwritten posts for the components sheet, modelled on the brief's examples
// and the stress cases in brief 14.8. Production content stays out of the repo.
//
// Every match in these posts is against one viewer: a Valorant player, a
// Controller at Diamond 1 in Croatia, speaking English, online 19:00–23:00, and
// looking for a ranked climb.

const VIEWER = "Player · Controller · Diamond 1 · Croatia · EN · 19:00–23:00 · Ranked climb";

const discord = { label: "Add on Discord", icon: "discord" };
const message = { label: "Message", icon: "message-circle" };
const contact = { label: "Contact", icon: "message-circle" };
const joinDiscord = { label: "Join Discord", icon: "discord" };

const FIXTURES = {
    shadowFox: {
        type: "player",
        name: "ShadowFox",
        freshness: "Active 2 days ago",
        facts: [
            { text: "Diamond 2", match: "fit" },
            { text: "Duelist, Initiator", match: "miss" },
            { text: "Croatia", match: "fit" },
            { text: "EN, DE", match: "fit" },
            { icon: "mic", label: "Microphone", match: "fit" },
            { text: "Ranked climb", match: "fit" },
            { text: "Returning player" },
            { text: "19:00–23:00", tabular: true, match: "fit", comparedOnly: true },
        ],
        text: "Peak Immortal last act, looking for a consistent duo. Chill but I want to improve, happy to review VODs together. I play most evenings after work and I'm free all weekend.",
        details: [
            { label: "Usually online", value: "19:00–23:00" },
            { label: "Age", value: "24" },
            { label: "Agents", value: "Jett, Raze, Sova" },
            { label: "Playstyle", value: "Aggressive" },
        ],
        trackers: ["tracker.gg/shadowfox"],
        contact: discord,
    },

    nightOwls: {
        type: "group",
        name: "Night Owls",
        owner: "Kestrel",
        freshness: "Active 5 hours ago",
        slots: { members: 3, total: 5 },
        facts: [
            { text: "Platinum 1 – Diamond 3", match: "fit" },
            { text: "Needs Controller, Sentinel", match: "fit" },
            { text: "EU", match: "fit" },
            { text: "EN", match: "fit" },
            { icon: "mic", label: "Microphone required", match: "fit" },
            { text: "Ages 18+", match: "fit" },
            { text: "Ranked climb", match: "fit" },
            { text: "21:00–01:00", tabular: true, match: "fit", comparedOnly: true },
        ],
        text: "Three friends who play most nights, we want to stop solo queuing for the last two spots. No tilt, comms on, we review our losses on Sundays.",
        details: [{ label: "Usually online", value: "21:00–01:00" }, { label: "Server", value: "Frankfurt" }],
        contact: message,
    },

    radiantRising: {
        type: "community",
        name: "Radiant Rising",
        owner: "Mira",
        freshness: "Active 3 days ago",
        facts: [
            { text: "Discord server" },
            { text: "EU", match: "fit" },
            { text: "EN", match: "fit" },
            { text: "PC", match: "fit" },
            { text: "Ranked climb, Competitive, Leagues", match: "fit" },
            { text: "All experience levels" },
        ],
        text: "An EU Valorant community of about 400 players. We run in-house 10-mans every Friday, a monthly cup with small prizes, and coaching nights where our Immortal and Radiant members review your VODs. Find a duo in #lfg, join a scrim team, or just hang out in voice. New players are paired with a buddy for their first week so nobody gets lost.",
        details: [{ label: "Server", value: "Frankfurt, Paris" }],
        contact: joinDiscord,
    },

    afterglow: {
        type: "group",
        name: "Afterglow",
        owner: "Tomo",
        freshness: "Active 1 day ago",
        slots: { members: 4, total: 5 },
        facts: [
            { text: "Platinum 1 – Platinum 2", match: "miss" },
            { text: "Needs Controller", match: "fit" },
            { text: "EU", match: "fit" },
            { text: "EN", match: "fit" },
            { text: "Ranked climb", match: "fit" },
            { text: "20:00–23:00", tabular: true, match: "fit", comparedOnly: true },
        ],
        text: "Four of us from the same uni, looking for a Controller who can make it three evenings a week.",
        details: [{ label: "Usually online", value: "20:00–23:00" }],
        contact: contact,
    },

    lumen: {
        type: "player",
        name: "Lumen",
        freshness: "Active 6 days ago",
        facts: [
            { text: "Bronze 2", match: "miss" },
            { text: "Controller", match: "fit" },
            { text: "Portugal", match: "fit" },
            { text: "PT", match: "miss" },
            { text: "Casual", match: "miss" },
            { text: "08:00–12:00", tabular: true, match: "miss", comparedOnly: true },
        ],
        text: "Jogo de manhã antes das aulas, procuro alguém para jogar sem pressão.",
        details: [{ label: "Usually online", value: "08:00–12:00" }],
        contact: discord,
    },

    expiredPlayer: {
        type: "player",
        name: "Quill",
        freshness: "Active 3 months ago",
        expired: true,
        facts: [
            { text: "Gold 3", match: "miss" },
            { text: "Sentinel", match: "miss" },
            { text: "Slovenia", match: "fit" },
            { text: "EN", match: "fit" },
            { text: "Ranked climb", match: "fit" },
        ],
        text: "LF non-toxic mates for comp",
        contact: discord,
    },

    expiredGroup: {
        type: "group",
        owner: "Brann",
        freshness: "Active 1 year ago",
        expired: true,
        slots: { members: 2, total: 5 },
        facts: [
            { text: "Silver 1 – Gold 3", match: "miss" },
            { text: "Needs Controller, Initiator, Duelist", match: "fit" },
            { text: "EU", match: "fit" },
            { text: "EN, HR", match: "fit" },
            { text: "Casual", match: "miss" },
        ],
        text: "Two of us, chill games in the evening.",
        contact: message,
    },

    farlands: {
        type: "community",
        name: "The Farlands",
        owner: "Eirik",
        freshness: "Active 1 week ago",
        facts: [
            { text: "Dedicated server" },
            { text: "EU" },
            { text: "EN" },
            { text: "PC" },
            { text: "PvE, Building" },
            { text: "All experience levels" },
        ],
        text: "Looking for a fun and friendly Valheim community server? Join The Farlands: weekly boss raids, a trading hub, building contests and 100-player events. New Vikings get a starter kit and a guide to the first biomes, and our admins are online most evenings to help.\n\nWe wipe once a year with a big send-off event, and the whole map history is kept in our Discord gallery.",
        details: [
            { label: "Server type", value: "Modded" },
            { label: "Server characters", value: "New characters" },
        ],
        contact: joinDiscord,
    },

    valheimGroup: {
        type: "group",
        owner: "Sigrun",
        freshness: "Active 3 hours ago",
        slots: { wants: "2–3" },
        facts: [
            { text: "EU" },
            { text: "EN, SV" },
            { icon: "mic", label: "Microphone required" },
            { text: "PvE, Building" },
        ],
        text: "My wife and I are starting over on a new server and want a few more players. We build a lot, fight bosses together and don't rush.",
        details: [{ label: "Usually online", value: "19:00–23:00" }],
        contact: message,
    },

    stress: {
        type: "player",
        name: "xX_NightmareOfTheEasternFront_Xx",
        freshness: "Active 20 minutes ago",
        facts: [
            { text: "Immortal 3" },
            { text: "Duelist, Initiator, Controller, Sentinel" },
            { text: "Russia" },
            { text: "RU, EN, UK" },
            { text: "Ranked climb, Competitive, Leagues" },
            { text: "Returning player" },
        ],
        text: "Ищу команду для рейтинговых игр, играю в основном вечером по Москве. Спокойный, без токсичности, микрофон есть. Могу играть на любой роли, но лучше всего на Контроллере и Инициаторе.",
        contact: discord,
    },

    sparse: {
        type: "player",
        name: "Ngọc Anh",
        freshness: "Active 4 days ago",
        facts: [
            { text: "Silver 1" },
            { text: "Vietnam" },
            { text: "VI" },
        ],
        text: "",
        contact: discord,
    },
};
