-- League of Legends
--
-- Ranks carry their divisions, "Gold II" as the brief's cards read them
-- (brief 5.1), so a rank range spans divisions and rank closeness counts in
-- steps of one (brief 7.2). Divisions are numbered IV up to I, so the ordinals
-- run Iron IV, Iron III, Iron II, Iron I, Bronze IV and on: worst to best, the
-- reverse of the numbering inside each tier. Master, Grandmaster and Challenger
-- have no divisions and are one option each.
--
-- Roles are the five positions, in lane order and under the names League
-- players post them by. They are slotted: two players fit by covering two
-- different ones, which is what a Mid and a Jungler are. League players don't
-- name a shotcaller as a slot, so there is no in-game leader field.
--
-- Looking for is the shared intents, ARAM and the other fun modes under Casual
-- and ranked flex under Ranked, then Clash: a premade five's bracket on a
-- regular schedule, recruited for by name.
--
-- Clash tier is the bracket a team is seeded into, numbered like the divisions
-- with I at the top, so its ordinals run IV up to I as well. It is a second
-- ordered field rather than the rank, so it waits behind Details, and a
-- community is never in one.
--
-- League runs on PC alone, so there is no platform field.

insert into game (title, short_title, handle, description)
values
    ( 'League of Legends'
    , 'LoL'
    , 'league-of-legends'
    , array['Find League of Legends players, groups and communities: a duo to climb with, a five stack for Clash, a team for scrims and tournaments, or a place to play ARAM in.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('riot')) as contact (kind)
where game.handle = 'league-of-legends';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true,  1),
    ('role',        'Role',        'multi',  false, true,  array['player', 'group'],              true,  2),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true,  3),
    ('clash-tier',  'Clash tier',  'single', true,  false, array['player', 'group'],              false, 4)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'league-of-legends';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'iron-iv',       'Iron IV',       1),
    ('rank', 'iron-iii',      'Iron III',      2),
    ('rank', 'iron-ii',       'Iron II',       3),
    ('rank', 'iron-i',        'Iron I',        4),
    ('rank', 'bronze-iv',     'Bronze IV',     5),
    ('rank', 'bronze-iii',    'Bronze III',    6),
    ('rank', 'bronze-ii',     'Bronze II',     7),
    ('rank', 'bronze-i',      'Bronze I',      8),
    ('rank', 'silver-iv',     'Silver IV',     9),
    ('rank', 'silver-iii',    'Silver III',    10),
    ('rank', 'silver-ii',     'Silver II',     11),
    ('rank', 'silver-i',      'Silver I',      12),
    ('rank', 'gold-iv',       'Gold IV',       13),
    ('rank', 'gold-iii',      'Gold III',      14),
    ('rank', 'gold-ii',       'Gold II',       15),
    ('rank', 'gold-i',        'Gold I',        16),
    ('rank', 'platinum-iv',   'Platinum IV',   17),
    ('rank', 'platinum-iii',  'Platinum III',  18),
    ('rank', 'platinum-ii',   'Platinum II',   19),
    ('rank', 'platinum-i',    'Platinum I',    20),
    ('rank', 'emerald-iv',    'Emerald IV',    21),
    ('rank', 'emerald-iii',   'Emerald III',   22),
    ('rank', 'emerald-ii',    'Emerald II',    23),
    ('rank', 'emerald-i',     'Emerald I',     24),
    ('rank', 'diamond-iv',    'Diamond IV',    25),
    ('rank', 'diamond-iii',   'Diamond III',   26),
    ('rank', 'diamond-ii',    'Diamond II',    27),
    ('rank', 'diamond-i',     'Diamond I',     28),
    ('rank', 'master',        'Master',        29),
    ('rank', 'grandmaster',   'Grandmaster',   30),
    ('rank', 'challenger',    'Challenger',    31),

    ('role', 'top',     'Top',     1),
    ('role', 'jungle',  'Jungle',  2),
    ('role', 'mid',     'Mid',     3),
    ('role', 'adc',     'ADC',     4),
    ('role', 'support', 'Support', 5),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'clash',              'Clash',                  5),

    ('clash-tier', 'tier-iv',  'Tier IV',  1),
    ('clash-tier', 'tier-iii', 'Tier III', 2),
    ('clash-tier', 'tier-ii',  'Tier II',  3),
    ('clash-tier', 'tier-i',   'Tier I',   4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'league-of-legends';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    -- op.gg's profiles are per region, and this search resolves the Riot ID to
    -- the right one, so the template needs no region the post cannot give.
    ('riot', 'op.gg',      'https://op.gg/summoners/search?q='),
    ('riot', 'tracker.gg', 'https://tracker.gg/lol/profile/riot/')
) as tracker (contact_kind, title, template)
where game.handle = 'league-of-legends';
