-- Marvel Rivals
--
-- Ranks carry their divisions, "Diamond II" as the game writes them, so a rank
-- range spans divisions and rank closeness counts in steps of one (brief 7.2).
-- Divisions are numbered III up to I, so the ordinals run Bronze III, Bronze
-- II, Bronze I, Silver III and on: worst to best, the reverse of the numbering
-- inside each tier. Eternity and One Above All have no divisions and are one
-- option each. There is one rank across all three roles.
--
-- Roles are the three the game sorts its heroes into, labelled as players
-- write them: Tank, DPS and Support, not Vanguard, Duelist and Strategist. They
-- are slotted: two players fit by covering two different ones. Players don't
-- name a shotcaller, so there is no in-game leader field.
--
-- Server is which of the game's servers a post plays on, not where its players
-- live: that is the post's own regions field. The lobby asks for one, a party
-- plays on it together, and players on the edge of a region name the one they
-- pick, Dammam or Singapore, Dallas or Virginia.
--
-- The Marvel Rivals Championship is the one team format in Looking for: the
-- in-game tournament every season, played by a Faction formed ahead of time
-- and recruited for by name.
--
-- Marvel Rivals runs on PC, PlayStation and Xbox. Crossplay is everywhere
-- outside competitive, where PC and the consoles are separate pools, so what a
-- post plays on is worth a field here.
--
-- Players add each other by their Marvel Rivals username, the same on every
-- platform, and console players by their PSN ID or gamertag too. Nobody trades
-- a Steam profile: the game's friend list is its own.

insert into game (title, short_title, handle, description)
values
    ( 'Marvel Rivals'
    , 'Marvel Rivals'
    , 'marvel-rivals'
    , array['Find Marvel Rivals players, groups and communities: a duo for quick play, a stack to climb ranked with, or a Faction for the Championship.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('marvel_rivals'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'marvel-rivals';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true,  1),
    ('role',        'Role',        'multi',  false, true,  array['player', 'group'],              true,  2),
    ('server',      'Server',      'multi',  false, false, array['player', 'group', 'community'], false, 3),
    ('platform',    'Platform',    'multi',  false, false, array['player', 'group', 'community'], true,  4),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true,  5)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'marvel-rivals';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'bronze-iii',      'Bronze III',      1),
    ('rank', 'bronze-ii',       'Bronze II',       2),
    ('rank', 'bronze-i',        'Bronze I',        3),
    ('rank', 'silver-iii',      'Silver III',      4),
    ('rank', 'silver-ii',       'Silver II',       5),
    ('rank', 'silver-i',        'Silver I',        6),
    ('rank', 'gold-iii',        'Gold III',        7),
    ('rank', 'gold-ii',         'Gold II',         8),
    ('rank', 'gold-i',          'Gold I',          9),
    ('rank', 'platinum-iii',    'Platinum III',    10),
    ('rank', 'platinum-ii',     'Platinum II',     11),
    ('rank', 'platinum-i',      'Platinum I',      12),
    ('rank', 'diamond-iii',     'Diamond III',     13),
    ('rank', 'diamond-ii',      'Diamond II',      14),
    ('rank', 'diamond-i',       'Diamond I',       15),
    ('rank', 'grandmaster-iii', 'Grandmaster III', 16),
    ('rank', 'grandmaster-ii',  'Grandmaster II',  17),
    ('rank', 'grandmaster-i',   'Grandmaster I',   18),
    ('rank', 'celestial-iii',   'Celestial III',   19),
    ('rank', 'celestial-ii',    'Celestial II',    20),
    ('rank', 'celestial-i',     'Celestial I',     21),
    ('rank', 'eternity',        'Eternity',        22),
    ('rank', 'one-above-all',   'One Above All',   23),

    ('role', 'tank',    'Tank',    1),
    ('role', 'dps',     'DPS',     2),
    ('role', 'support', 'Support', 3),

    ('server', 'oregon',            'Oregon',            1),
    ('server', 'dallas',            'Dallas',            2),
    ('server', 'northern-virginia', 'Northern Virginia', 3),
    ('server', 'sao-paulo',         'São Paulo',         4),
    ('server', 'frankfurt',         'Frankfurt',         5),
    ('server', 'warsaw',            'Warsaw',            6),
    ('server', 'dammam',            'Dammam',            7),
    ('server', 'singapore',         'Singapore',         8),
    ('server', 'tokyo',             'Tokyo',             9),
    ('server', 'sydney',            'Sydney',            10),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',                     'Casual',                     1),
    ('looking-for', 'ranked',                     'Ranked',                     2),
    ('looking-for', 'scrims-tournaments',         'Scrims and tournaments',     3),
    ('looking-for', 'learning-the-game',          'Learning the game',          4),
    ('looking-for', 'marvel-rivals-championship', 'Marvel Rivals Championship', 5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'marvel-rivals';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('marvel_rivals', 'tracker.gg', 'https://tracker.gg/marvel-rivals/profile/ign/')
) as tracker (contact_kind, title, template)
where game.handle = 'marvel-rivals';
