-- Rust
--
-- Rust has no rank, and hours played stand in for one: posts give their hours
-- before anything else ("1.5k hours"), and groups recruit above a number of
-- them ("1k+ hours"). Hours played is ordered so a group can want a range, in
-- bands that split the hours posts give about evenly and read as players give
-- them, "2k hours" for anything from two thousand to just under three. A band
-- has no divisions, so rank closeness counts one band either way (brief 7.2).
--
-- Roles are the jobs a Rust team splits a wipe into, and clans recruit by them
-- with a count for each ("4 PvPers, 2 farmers, 1 electric, 2 builders"). They
-- are slotted: two players fit by covering two different ones. Industrial,
-- conveyors and sorting, is part of the electrician's job, as posts pair them.
--
-- Team size is the group a post plays as, which is also the limit a server
-- holds teams to: duo, trio and quad, and a clan for five or more. A solo has
-- no team to find, so it is not an option.
--
-- Server type is where a post plays, in the words posts use. Official is
-- Facepunch's own servers, Vanilla the community servers that run the game
-- unchanged, and 2x, 3x and 5x and up the modded servers that multiply what is
-- gathered, which players name by the rate. Modded is every other modded
-- server. Softcore, Primitive and Hardcore are hardly named, so they are not
-- asked.
--
-- Wipe is how often a post's server starts afresh, which servers advertise
-- themselves by and posts give beside the server. It waits behind Details.
--
-- Rust has no ranked play and no organised team play that posts recruit for,
-- so Looking for keeps the shared Casual and Learning the game and then names
-- the playstyles posts and servers give. Competitive is the organised,
-- high-population play that Casual stands against, under the word posts use
-- when they want it; sweaty and tryhard are mostly how posts turn it down. PvE
-- is the servers that turn player damage off.
--
-- Rust runs on PC and, as Rust Console Edition, on PlayStation and Xbox. The
-- consoles play together and apart from PC, so what a post plays on is worth a
-- field here. Console players give their PSN ID or gamertag, and PC players
-- their Steam profile, which bamboogaming.net looks their Rust stats up by.

insert into game (title, short_title, handle, description)
values
    ( 'Rust'
    , 'Rust'
    , 'rust'
    , array['Find Rust players, groups and communities: a duo or trio for wipe day, a clan to join, or a server to play on.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'rust';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('hours-played', 'Hours played', 'single', true,  false, array['player', 'group'],              true,  1),
    ('role',         'Role',         'multi',  false, true,  array['player', 'group'],              true,  2),
    ('team-size',    'Team size',    'multi',  false, false, array['player', 'group', 'community'], true,  3),
    ('server-type',  'Server type',  'multi',  false, false, array['player', 'group', 'community'], true,  4),
    ('platform',     'Platform',     'multi',  false, false, array['player', 'group', 'community'], true,  5),
    ('looking-for',  'Looking for',  'multi',  false, false, array['player', 'group', 'community'], true,  6),
    ('wipe',         'Wipe',         'multi',  false, false, array['player', 'group', 'community'], false, 7)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'rust';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('hours-played', 'under-500', 'Under 500 hours', 1),
    ('hours-played', '500',       '500 hours',       2),
    ('hours-played', '1k',        '1k hours',        3),
    ('hours-played', '2k',        '2k hours',        4),
    ('hours-played', '3k',        '3k hours',        5),
    ('hours-played', '5k',        '5k hours',        6),
    ('hours-played', '10k',       '10k hours',       7),

    ('role', 'pvper',       'PvPer',       1),
    ('role', 'farmer',      'Farmer',      2),
    ('role', 'builder',     'Builder',     3),
    ('role', 'electrician', 'Electrician', 4),

    ('team-size', 'duo',  'Duo',  1),
    ('team-size', 'trio', 'Trio', 2),
    ('team-size', 'quad', 'Quad', 3),
    ('team-size', 'clan', 'Clan', 4),

    ('server-type', 'official',  'Official',  1),
    ('server-type', 'vanilla',   'Vanilla',   2),
    ('server-type', '2x',        '2x',        3),
    ('server-type', '3x',        '3x',        4),
    ('server-type', '5x-and-up', '5x and up', 5),
    ('server-type', 'modded',    'Modded',    6),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',            'Casual',            1),
    ('looking-for', 'learning-the-game', 'Learning the game', 2),
    ('looking-for', 'competitive',       'Competitive',       3),
    ('looking-for', 'pve',               'PvE',               4),

    ('wipe', 'weekly',   'Weekly',   1),
    ('wipe', 'biweekly', 'Biweekly', 2),
    ('wipe', 'monthly',  'Monthly',  3)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'rust';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('steam', 'bamboogaming.net', 'https://www.bamboogaming.net/rust/stats?steamid=')
) as tracker (contact_kind, title, template)
where game.handle = 'rust';
