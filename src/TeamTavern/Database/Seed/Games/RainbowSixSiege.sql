-- Rainbow Six Siege
--
-- Ranks carry their divisions, numbered V down to I, so Copper V is the floor
-- and Champion I the ceiling (brief 5.1): a rank range spans divisions and rank
-- closeness counts in steps of one (brief 7.2). Emerald sits between Platinum
-- and Diamond. The Legend Division past Champion I is solo queue only and asks
-- for a Champion rank to enter, so nothing a post states ever lands there.
--
-- Roles are the jobs players name: entry and support on attack, roamer and
-- anchor on defence, with support played on both. Every player plays both
-- sides, so a player picks a job for each, and the field stays one list rather
-- than one per side, so a group short of only an entry answers one field. They
-- are slotted: two players fit by covering two different ones. Fragger is
-- folded into entry and hard breach and intel into support, as the posts use
-- them. In-game leader is its own yes-or-no field, since the caller still plays
-- a job on each side.
--
-- Siege runs on PC, PlayStation and Xbox, so what a post plays on is worth a
-- field here. The options are platform families, which is why the two console
-- generations are one option each.
--
-- Siege Cup is a team format: an official 5-stack tournament that runs every
-- few weekends, and players recruit for it by name.

insert into game (title, short_title, handle, description)
values
    ( 'Rainbow Six Siege'
    , 'R6S'
    , 'r6s'
    , array['Find Rainbow Six Siege players, groups and communities: a ranked duo, a fifth for your stack, a team for Siege Cup and scrims, or a clan to run with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam'), ('ubisoft'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'r6s';

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
where game.handle = 'r6s';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'copper-v',     'Copper V',     1),
    ('rank', 'copper-iv',    'Copper IV',    2),
    ('rank', 'copper-iii',   'Copper III',   3),
    ('rank', 'copper-ii',    'Copper II',    4),
    ('rank', 'copper-i',     'Copper I',     5),
    ('rank', 'bronze-v',     'Bronze V',     6),
    ('rank', 'bronze-iv',    'Bronze IV',    7),
    ('rank', 'bronze-iii',   'Bronze III',   8),
    ('rank', 'bronze-ii',    'Bronze II',    9),
    ('rank', 'bronze-i',     'Bronze I',     10),
    ('rank', 'silver-v',     'Silver V',     11),
    ('rank', 'silver-iv',    'Silver IV',    12),
    ('rank', 'silver-iii',   'Silver III',   13),
    ('rank', 'silver-ii',    'Silver II',    14),
    ('rank', 'silver-i',     'Silver I',     15),
    ('rank', 'gold-v',       'Gold V',       16),
    ('rank', 'gold-iv',      'Gold IV',      17),
    ('rank', 'gold-iii',     'Gold III',     18),
    ('rank', 'gold-ii',      'Gold II',      19),
    ('rank', 'gold-i',       'Gold I',       20),
    ('rank', 'platinum-v',   'Platinum V',   21),
    ('rank', 'platinum-iv',  'Platinum IV',  22),
    ('rank', 'platinum-iii', 'Platinum III', 23),
    ('rank', 'platinum-ii',  'Platinum II',  24),
    ('rank', 'platinum-i',   'Platinum I',   25),
    ('rank', 'emerald-v',    'Emerald V',    26),
    ('rank', 'emerald-iv',   'Emerald IV',   27),
    ('rank', 'emerald-iii',  'Emerald III',  28),
    ('rank', 'emerald-ii',   'Emerald II',   29),
    ('rank', 'emerald-i',    'Emerald I',    30),
    ('rank', 'diamond-v',    'Diamond V',    31),
    ('rank', 'diamond-iv',   'Diamond IV',   32),
    ('rank', 'diamond-iii',  'Diamond III',  33),
    ('rank', 'diamond-ii',   'Diamond II',   34),
    ('rank', 'diamond-i',    'Diamond I',    35),
    ('rank', 'champion-v',   'Champion V',   36),
    ('rank', 'champion-iv',  'Champion IV',  37),
    ('rank', 'champion-iii', 'Champion III', 38),
    ('rank', 'champion-ii',  'Champion II',  39),
    ('rank', 'champion-i',   'Champion I',   40),

    ('role', 'entry',   'Entry',   1),
    ('role', 'support', 'Support', 2),
    ('role', 'roamer',  'Roamer',  3),
    ('role', 'anchor',  'Anchor',  4),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'siege-cup',          'Siege Cup',              5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'r6s';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('ubisoft',   'r6.tracker.network', 'https://r6.tracker.network/r6siege/profile/ubi/'),
    ('psn',       'r6.tracker.network', 'https://r6.tracker.network/r6siege/profile/psn/'),
    ('gamer_tag', 'r6.tracker.network', 'https://r6.tracker.network/r6siege/profile/xbl/')
) as tracker (contact_kind, title, template)
where game.handle = 'r6s';
