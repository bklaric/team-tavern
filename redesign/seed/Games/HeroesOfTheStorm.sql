-- Heroes of the Storm
--
-- Ranks carry their divisions, five to a tier and numbered downwards so that
-- Bronze 5 is the bottom of Bronze and Bronze 1 the top, so a rank range spans
-- divisions and rank closeness counts in steps of one (brief 7.2). Master and
-- Grand Master have no divisions: Master is ordered by rank points and Grand
-- Master is the top hundred of a region's Master players.
--
-- Roles are Blizzard's six hero roles, which is what a Heroes player says they
-- play. They are slotted: two players fit by covering two different ones.
--
-- The game runs on PC only, so it has no platform field.
--
-- It also has no tracker. The stat sites that are still up key a player page by
-- region and an internal id rather than by BattleTag, so none of them can be
-- reached by appending a contact to a template.

insert into game (title, short_title, handle, description)
values
    ( 'Heroes of the Storm'
    , 'HotS'
    , 'hots'
    , array['Find Heroes of the Storm players, groups and communities: a Storm League core, an ARAM crew, or a Discord to draft with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('battle_tag')) as contact (kind)
where game.handle = 'hots';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true, 1),
    ('role',        'Role',        'multi',  false, true,  array['player', 'group'],              true, 2),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true, 3)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'hots';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'bronze-5',     'Bronze 5',     1),
    ('rank', 'bronze-4',     'Bronze 4',     2),
    ('rank', 'bronze-3',     'Bronze 3',     3),
    ('rank', 'bronze-2',     'Bronze 2',     4),
    ('rank', 'bronze-1',     'Bronze 1',     5),
    ('rank', 'silver-5',     'Silver 5',     6),
    ('rank', 'silver-4',     'Silver 4',     7),
    ('rank', 'silver-3',     'Silver 3',     8),
    ('rank', 'silver-2',     'Silver 2',     9),
    ('rank', 'silver-1',     'Silver 1',     10),
    ('rank', 'gold-5',       'Gold 5',       11),
    ('rank', 'gold-4',       'Gold 4',       12),
    ('rank', 'gold-3',       'Gold 3',       13),
    ('rank', 'gold-2',       'Gold 2',       14),
    ('rank', 'gold-1',       'Gold 1',       15),
    ('rank', 'platinum-5',   'Platinum 5',   16),
    ('rank', 'platinum-4',   'Platinum 4',   17),
    ('rank', 'platinum-3',   'Platinum 3',   18),
    ('rank', 'platinum-2',   'Platinum 2',   19),
    ('rank', 'platinum-1',   'Platinum 1',   20),
    ('rank', 'diamond-5',    'Diamond 5',    21),
    ('rank', 'diamond-4',    'Diamond 4',    22),
    ('rank', 'diamond-3',    'Diamond 3',    23),
    ('rank', 'diamond-2',    'Diamond 2',    24),
    ('rank', 'diamond-1',    'Diamond 1',    25),
    ('rank', 'master',       'Master',       26),
    ('rank', 'grand-master', 'Grand Master', 27),

    ('role', 'tank',            'Tank',            1),
    ('role', 'bruiser',         'Bruiser',         2),
    ('role', 'support',         'Support',         3),
    ('role', 'healer',          'Healer',          4),
    ('role', 'melee-assassin',  'Melee Assassin',  5),
    ('role', 'ranged-assassin', 'Ranged Assassin', 6),

    ('looking-for', 'casual',       'Casual',       1),
    ('looking-for', 'ranked-climb', 'Ranked climb', 2),
    ('looking-for', 'aram',         'ARAM',         3),
    ('looking-for', 'tournaments',  'Tournaments',  4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'hots';
