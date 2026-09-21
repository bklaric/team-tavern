-- Apex Legends
--
-- One rank ladder, not two: Respawn sunset Arenas and its separate ranked mode,
-- and nothing ranked replaced it, so the rank field is the battle royale ladder
-- and leads the card on its own.
--
-- Ranks carry their divisions, in the Roman numerals the game writes them in.
-- A division counts down towards the next tier, so Bronze IV is the worst of
-- the Bronze options and Bronze I the best, and the ordinals run that way: a
-- rank range spans divisions and rank closeness counts in steps of one
-- (brief 7.2). Rookie, Master and Apex Predator have no divisions.
--
-- Roles are the jobs a squad splits, fragger and support, not the legend
-- classes: Apex players name their legends, and the class names are nobody's
-- vocabulary, while the posts that state a job say fragger or support, with
-- anchor as another word for support and refrag for a second fragger. They are
-- slotted: two players fit by covering two different ones. In-game leader is
-- its own yes-or-no field, since the caller still frags or supports: it says
-- a player can lead, and fits by agreeing rather than as a slot.
--
-- Switch means Switch 2. Support for the original Switch ended with Season 29,
-- and everything else runs on PC, PlayStation and Xbox, so what a post plays on
-- is worth a field here.

insert into game (title, short_title, handle, description)
values
    ( 'Apex Legends'
    , 'Apex Legends'
    , 'apex'
    , array['Find Apex Legends players, groups and communities: a third for your trio, a squad to climb ranked with, a team for scrims and the Challenger Circuit, or a community to drop with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values
    ('discord'), ('ea'), ('steam'), ('psn'), ('gamer_tag'), ('friend_code')
) as contact (kind)
where game.handle = 'apex';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',           'Rank',           'single',  true,  false, array['player', 'group'],              true,  1),
    ('role',           'Role',           'multi',   false, true,  array['player', 'group'],              true,  2),
    ('in-game-leader', 'In-game leader', 'boolean', false, false, array['player', 'group'],              false, 3),
    ('platform',       'Platform',       'multi',   false, false, array['player', 'group', 'community'], true,  4),
    ('looking-for',    'Looking for',    'multi',   false, false, array['player', 'group', 'community'], true,  5)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'apex';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'rookie',        'Rookie',          1),
    ('rank', 'bronze-iv',     'Bronze IV',       2),
    ('rank', 'bronze-iii',    'Bronze III',      3),
    ('rank', 'bronze-ii',     'Bronze II',       4),
    ('rank', 'bronze-i',      'Bronze I',        5),
    ('rank', 'silver-iv',     'Silver IV',       6),
    ('rank', 'silver-iii',    'Silver III',      7),
    ('rank', 'silver-ii',     'Silver II',       8),
    ('rank', 'silver-i',      'Silver I',        9),
    ('rank', 'gold-iv',       'Gold IV',         10),
    ('rank', 'gold-iii',      'Gold III',        11),
    ('rank', 'gold-ii',       'Gold II',         12),
    ('rank', 'gold-i',        'Gold I',          13),
    ('rank', 'platinum-iv',   'Platinum IV',     14),
    ('rank', 'platinum-iii',  'Platinum III',    15),
    ('rank', 'platinum-ii',   'Platinum II',     16),
    ('rank', 'platinum-i',    'Platinum I',      17),
    ('rank', 'diamond-iv',    'Diamond IV',      18),
    ('rank', 'diamond-iii',   'Diamond III',     19),
    ('rank', 'diamond-ii',    'Diamond II',      20),
    ('rank', 'diamond-i',     'Diamond I',       21),
    ('rank', 'master',        'Master',          22),
    ('rank', 'apex-predator', 'Apex Predator',   23),

    ('role', 'fragger', 'Fragger', 1),
    ('role', 'support', 'Support', 2),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),
    ('platform', 'switch',      'Switch',      4),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'apex';

-- tracker.gg looks an Apex player up by the account they play on, so there is
-- one tracker per account it can find. It holds no Steam profiles: a Steam
-- player is looked up by the EA account the game is played through anyway.

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('ea',        'tracker.gg', 'https://apex.tracker.gg/profile/origin/'),
    ('psn',       'tracker.gg', 'https://apex.tracker.gg/profile/psn/'),
    ('gamer_tag', 'tracker.gg', 'https://apex.tracker.gg/profile/xbl/')
) as tracker (contact_kind, title, template)
where game.handle = 'apex';
