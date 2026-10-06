-- Deadlock
--
-- Ranks carry their subranks, "Oracle IV" as Valve and Statlocker write them,
-- so a rank range spans subranks and rank closeness counts in steps of one
-- (brief 7.2). Every rank has six, I to VI, Eternus included: its subranks are
-- percentiles of the players in it rather than points, but they are still a
-- ladder. Obscurus is the badge of a player who hasn't calibrated, not a rank,
-- so it is not here.
--
-- There is no role field. Deadlock has no role queue, and its LFG posts seldom
-- state a role: when they do, it is a carry, a frontline, a support, an M1 or a
-- position from 1 to 6, with no list that two posts share. They name heroes
-- about as often. Nobody recruits a shotcaller either, so there is no in-game
-- leader field.
--
-- Standard and Street Brawl are what a post after Casual plays, and Ranked
-- takes solos and duos only. Scrims and tournaments is the team play outside
-- the queues, all of it run by the community, so there is no team format.
--
-- Deadlock runs on PC alone, so there is no platform field.
--
-- Players add each other on Steam, where Deadlock's invites are sent too, so
-- Steam is the account a post offers. Every tracker here finds a player's
-- profile from their SteamID64, and Statlocker comes first because it is the
-- one players link in their posts.

insert into game (title, short_title, handle, description)
values
    ( 'Deadlock'
    , 'Deadlock'
    , 'deadlock'
    , array['Find Deadlock players, groups and communities: a duo for Standard or ranked, a six stack, a team for scrims, or someone to learn the game with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam')) as contact (kind)
where game.handle = 'deadlock';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true, 1),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true, 2)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'deadlock';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'initiate-i',     'Initiate I',     1),
    ('rank', 'initiate-ii',    'Initiate II',    2),
    ('rank', 'initiate-iii',   'Initiate III',   3),
    ('rank', 'initiate-iv',    'Initiate IV',    4),
    ('rank', 'initiate-v',     'Initiate V',     5),
    ('rank', 'initiate-vi',    'Initiate VI',    6),
    ('rank', 'seeker-i',       'Seeker I',       7),
    ('rank', 'seeker-ii',      'Seeker II',      8),
    ('rank', 'seeker-iii',     'Seeker III',     9),
    ('rank', 'seeker-iv',      'Seeker IV',      10),
    ('rank', 'seeker-v',       'Seeker V',       11),
    ('rank', 'seeker-vi',      'Seeker VI',      12),
    ('rank', 'acolyte-i',      'Acolyte I',      13),
    ('rank', 'acolyte-ii',     'Acolyte II',     14),
    ('rank', 'acolyte-iii',    'Acolyte III',    15),
    ('rank', 'acolyte-iv',     'Acolyte IV',     16),
    ('rank', 'acolyte-v',      'Acolyte V',      17),
    ('rank', 'acolyte-vi',     'Acolyte VI',     18),
    ('rank', 'sentinel-i',     'Sentinel I',     19),
    ('rank', 'sentinel-ii',    'Sentinel II',    20),
    ('rank', 'sentinel-iii',   'Sentinel III',   21),
    ('rank', 'sentinel-iv',    'Sentinel IV',    22),
    ('rank', 'sentinel-v',     'Sentinel V',     23),
    ('rank', 'sentinel-vi',    'Sentinel VI',    24),
    ('rank', 'mystic-i',       'Mystic I',       25),
    ('rank', 'mystic-ii',      'Mystic II',      26),
    ('rank', 'mystic-iii',     'Mystic III',     27),
    ('rank', 'mystic-iv',      'Mystic IV',      28),
    ('rank', 'mystic-v',       'Mystic V',       29),
    ('rank', 'mystic-vi',      'Mystic VI',      30),
    ('rank', 'ritualist-i',    'Ritualist I',    31),
    ('rank', 'ritualist-ii',   'Ritualist II',   32),
    ('rank', 'ritualist-iii',  'Ritualist III',  33),
    ('rank', 'ritualist-iv',   'Ritualist IV',   34),
    ('rank', 'ritualist-v',    'Ritualist V',    35),
    ('rank', 'ritualist-vi',   'Ritualist VI',   36),
    ('rank', 'emissary-i',     'Emissary I',     37),
    ('rank', 'emissary-ii',    'Emissary II',    38),
    ('rank', 'emissary-iii',   'Emissary III',   39),
    ('rank', 'emissary-iv',    'Emissary IV',    40),
    ('rank', 'emissary-v',     'Emissary V',     41),
    ('rank', 'emissary-vi',    'Emissary VI',    42),
    ('rank', 'oracle-i',       'Oracle I',       43),
    ('rank', 'oracle-ii',      'Oracle II',      44),
    ('rank', 'oracle-iii',     'Oracle III',     45),
    ('rank', 'oracle-iv',      'Oracle IV',      46),
    ('rank', 'oracle-v',       'Oracle V',       47),
    ('rank', 'oracle-vi',      'Oracle VI',      48),
    ('rank', 'phantom-i',      'Phantom I',      49),
    ('rank', 'phantom-ii',     'Phantom II',     50),
    ('rank', 'phantom-iii',    'Phantom III',    51),
    ('rank', 'phantom-iv',     'Phantom IV',     52),
    ('rank', 'phantom-v',      'Phantom V',      53),
    ('rank', 'phantom-vi',     'Phantom VI',     54),
    ('rank', 'ascendant-i',    'Ascendant I',    55),
    ('rank', 'ascendant-ii',   'Ascendant II',   56),
    ('rank', 'ascendant-iii',  'Ascendant III',  57),
    ('rank', 'ascendant-iv',   'Ascendant IV',   58),
    ('rank', 'ascendant-v',    'Ascendant V',    59),
    ('rank', 'ascendant-vi',   'Ascendant VI',   60),
    ('rank', 'eternus-i',      'Eternus I',      61),
    ('rank', 'eternus-ii',     'Eternus II',     62),
    ('rank', 'eternus-iii',    'Eternus III',    63),
    ('rank', 'eternus-iv',     'Eternus IV',     64),
    ('rank', 'eternus-v',      'Eternus V',      65),
    ('rank', 'eternus-vi',     'Eternus VI',     66),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'deadlock';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('steam', 'statlocker.gg',        'https://statlocker.gg/profile/'),
    ('steam', 'tracker.gg',           'https://tracker.gg/deadlock/profile/steam/'),
    ('steam', 'mobalytics.gg',        'https://mobalytics.gg/deadlock/player-profile/'),
    ('steam', 'deadlock-tracker.com', 'https://deadlock-tracker.com/players/')
) as tracker (contact_kind, title, template)
where game.handle = 'deadlock';
