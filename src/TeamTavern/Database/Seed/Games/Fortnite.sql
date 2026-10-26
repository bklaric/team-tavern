-- Fortnite
--
-- Rank is per ladder: Battle Royale keeps one and Reload another, and a player
-- states the ranks of the ones they play ("unreal in reload, elite in br"), so
-- the game has a rank field per ladder. Ranked Reload draws as many players as
-- ranked Battle Royale, so both lead the card, and a post that plays one fills
-- one. Battle Royale and Zero Build share the Battle Royale ladder, as Reload
-- and its Zero Build share Reload's.
--
-- Ranks carry their divisions, in the Roman numerals the game writes them in,
-- so a rank range spans divisions and rank closeness counts in steps of one
-- (brief 7.2). Unreal is the one rank without them.
--
-- Unreal Legends, which Epic's tournament rules list as a 23rd division above
-- Unreal, is not an option. A player holds it only while in the top 500 of the
-- Unreal leaderboard with a Skill Rating of at least 2,500, and loses it the
-- moment either slips, so a post naming it goes stale within days, while every
-- other rank holds until the season's reset. Everyone who holds it is Unreal,
-- which their posts say.
--
-- Building is whether a post plays with building or Zero Build. Ranked mixes
-- the two on one ladder, but a match is one or the other, so it is a fact two
-- posts share, and LFG posts and servers lead with it ("[ZB]", ranked-zb-na
-- beside ranked-builds-na).
--
-- Roles are the jobs a competitive duo or trio splits, fragger and support, as
-- its posts name them; casual posts seldom name one. They are slotted: two
-- players fit by covering two different ones. In-game leader is its own
-- yes-or-no field, since the caller still frags or supports ("mainly a
-- support/IGL", "igl but can co-frag"): it says a player can lead, and two
-- players fit when either can.
--
-- Server is which of the game's regions a post plays on, not where its players
-- live: that is the post's own regions field. Matchmaking is one North America
-- region with a preferred subregion, and tournaments are run per subregion, so
-- North American posts name East, Central or West, which the North America
-- region can't say.
--
-- FNCS is the one team format in Looking for: Epic's championship series, with
-- divisional cups through the season, entered by a team formed ahead of time
-- and recruited for by name. The cash, skin and ranked cups are Scrims and
-- tournaments, which is what posts recruiting for them say.
--
-- Fortnite runs on PC, PlayStation, Xbox and Switch, with crossplay everywhere,
-- ranked included. What a post plays on decides which account it offers and
-- whether a party can use its console's own chat, so it is still a field here.
--
-- The friend list is Epic's on every platform, so players add each other by
-- Epic ID, and console players by their PSN ID, gamertag or friend code too.

insert into game (title, short_title, handle, description)
values
    ( 'Fortnite'
    , 'Fortnite'
    , 'fortnite'
    , array['Find Fortnite players, groups and communities: a duo for ranked, a squad for Zero Build, a fragger or IGL for cups and FNCS, or a clan to join.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('epic'), ('psn'), ('gamer_tag'), ('friend_code')) as contact (kind)
where game.handle = 'fortnite';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('battle-royale-rank', 'Battle Royale rank', 'single',  true,  false, array['player', 'group'],              true,  1),
    ('reload-rank',        'Reload rank',        'single',  true,  false, array['player', 'group'],              true,  2),
    ('role',               'Role',               'multi',   false, true,  array['player', 'group'],              true,  3),
    ('in-game-leader',     'In-game leader',     'boolean', false, false, array['player', 'group'],              false, 4),
    ('building',           'Building',           'multi',   false, false, array['player', 'group', 'community'], true,  5),
    ('server',             'Server',             'multi',   false, false, array['player', 'group', 'community'], false, 6),
    ('platform',           'Platform',           'multi',   false, false, array['player', 'group', 'community'], true,  7),
    ('looking-for',        'Looking for',        'multi',   false, false, array['player', 'group', 'community'], true,  8)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'fortnite';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('battle-royale-rank', 'bronze-i',     'Bronze I',     1),
    ('battle-royale-rank', 'bronze-ii',    'Bronze II',    2),
    ('battle-royale-rank', 'bronze-iii',   'Bronze III',   3),
    ('battle-royale-rank', 'silver-i',     'Silver I',     4),
    ('battle-royale-rank', 'silver-ii',    'Silver II',    5),
    ('battle-royale-rank', 'silver-iii',   'Silver III',   6),
    ('battle-royale-rank', 'gold-i',       'Gold I',       7),
    ('battle-royale-rank', 'gold-ii',      'Gold II',      8),
    ('battle-royale-rank', 'gold-iii',     'Gold III',     9),
    ('battle-royale-rank', 'platinum-i',   'Platinum I',   10),
    ('battle-royale-rank', 'platinum-ii',  'Platinum II',  11),
    ('battle-royale-rank', 'platinum-iii', 'Platinum III', 12),
    ('battle-royale-rank', 'diamond-i',    'Diamond I',    13),
    ('battle-royale-rank', 'diamond-ii',   'Diamond II',   14),
    ('battle-royale-rank', 'diamond-iii',  'Diamond III',  15),
    ('battle-royale-rank', 'elite-i',      'Elite I',      16),
    ('battle-royale-rank', 'elite-ii',     'Elite II',     17),
    ('battle-royale-rank', 'elite-iii',    'Elite III',    18),
    ('battle-royale-rank', 'champion-i',   'Champion I',   19),
    ('battle-royale-rank', 'champion-ii',  'Champion II',  20),
    ('battle-royale-rank', 'champion-iii', 'Champion III', 21),
    ('battle-royale-rank', 'unreal',       'Unreal',       22),

    ('reload-rank',        'bronze-i',     'Bronze I',     1),
    ('reload-rank',        'bronze-ii',    'Bronze II',    2),
    ('reload-rank',        'bronze-iii',   'Bronze III',   3),
    ('reload-rank',        'silver-i',     'Silver I',     4),
    ('reload-rank',        'silver-ii',    'Silver II',    5),
    ('reload-rank',        'silver-iii',   'Silver III',   6),
    ('reload-rank',        'gold-i',       'Gold I',       7),
    ('reload-rank',        'gold-ii',      'Gold II',      8),
    ('reload-rank',        'gold-iii',     'Gold III',     9),
    ('reload-rank',        'platinum-i',   'Platinum I',   10),
    ('reload-rank',        'platinum-ii',  'Platinum II',  11),
    ('reload-rank',        'platinum-iii', 'Platinum III', 12),
    ('reload-rank',        'diamond-i',    'Diamond I',    13),
    ('reload-rank',        'diamond-ii',   'Diamond II',   14),
    ('reload-rank',        'diamond-iii',  'Diamond III',  15),
    ('reload-rank',        'elite-i',      'Elite I',      16),
    ('reload-rank',        'elite-ii',     'Elite II',     17),
    ('reload-rank',        'elite-iii',    'Elite III',    18),
    ('reload-rank',        'champion-i',   'Champion I',   19),
    ('reload-rank',        'champion-ii',  'Champion II',  20),
    ('reload-rank',        'champion-iii', 'Champion III', 21),
    ('reload-rank',        'unreal',       'Unreal',       22),

    ('role', 'fragger', 'Fragger', 1),
    ('role', 'support', 'Support', 2),

    ('building', 'build',      'Build',      1),
    ('building', 'zero-build', 'Zero Build', 2),

    ('server', 'na-west',     'NA-West',     1),
    ('server', 'na-central',  'NA-Central',  2),
    ('server', 'na-east',     'NA-East',     3),
    ('server', 'brazil',      'Brazil',      4),
    ('server', 'europe',      'Europe',      5),
    ('server', 'middle-east', 'Middle East', 6),
    ('server', 'asia',        'Asia',        7),
    ('server', 'oceania',     'Oceania',     8),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),
    ('platform', 'switch',      'Switch',      4),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'fncs',               'FNCS',                   5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'fortnite';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('epic', 'fortnitetracker.com', 'https://fortnitetracker.com/fortnite/profile/epic/'),
    ('epic', 'fortnite.gg',         'https://fortnite.gg/stats?player=')
) as tracker (contact_kind, title, template)
where game.handle = 'fortnite';
