-- Valorant
--
-- Ranks carry their divisions, "Diamond 2" as the brief's cards read them
-- (brief 5.1), so a rank range spans divisions and rank closeness counts in
-- steps of one (brief 7.2). Radiant is the one tier without them.
--
-- Roles are the agent roles, which is what a Valorant player says they play,
-- under their official names: players write Duelist, Initiator and Sentinel far
-- more than entry, init or senti, and Controller as often as smokes. They are
-- slotted: two players fit by covering two different ones.
--
-- In-game leader is asked beside the roles because teams recruit for one by
-- name. It says a player can lead on top of the role they play, and any number
-- can, so it is not a slot: two players fit when either can lead.
--
-- Premier is the one team format in Looking for: Riot's weekly team league,
-- played by a team formed ahead of time and recruited for by name.
--
-- Valorant runs on PC, PlayStation and Xbox, and crossplay is between the two
-- consoles only, so what a post plays on is worth a field here.

insert into game (title, short_title, handle, description)
values
    ( 'Valorant'
    , 'Valorant'
    , 'valorant'
    , array['Find Valorant players, groups and communities: a duo, a five stack, a Premier team, or a server to play on.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('riot')) as contact (kind)
where game.handle = 'valorant';

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
where game.handle = 'valorant';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'iron-1',        'Iron 1',        1),
    ('rank', 'iron-2',        'Iron 2',        2),
    ('rank', 'iron-3',        'Iron 3',        3),
    ('rank', 'bronze-1',      'Bronze 1',      4),
    ('rank', 'bronze-2',      'Bronze 2',      5),
    ('rank', 'bronze-3',      'Bronze 3',      6),
    ('rank', 'silver-1',      'Silver 1',      7),
    ('rank', 'silver-2',      'Silver 2',      8),
    ('rank', 'silver-3',      'Silver 3',      9),
    ('rank', 'gold-1',        'Gold 1',        10),
    ('rank', 'gold-2',        'Gold 2',        11),
    ('rank', 'gold-3',        'Gold 3',        12),
    ('rank', 'platinum-1',    'Platinum 1',    13),
    ('rank', 'platinum-2',    'Platinum 2',    14),
    ('rank', 'platinum-3',    'Platinum 3',    15),
    ('rank', 'diamond-1',     'Diamond 1',     16),
    ('rank', 'diamond-2',     'Diamond 2',     17),
    ('rank', 'diamond-3',     'Diamond 3',     18),
    ('rank', 'ascendant-1',   'Ascendant 1',   19),
    ('rank', 'ascendant-2',   'Ascendant 2',   20),
    ('rank', 'ascendant-3',   'Ascendant 3',   21),
    ('rank', 'immortal-1',    'Immortal 1',    22),
    ('rank', 'immortal-2',    'Immortal 2',    23),
    ('rank', 'immortal-3',    'Immortal 3',    24),
    ('rank', 'radiant',       'Radiant',       25),

    ('role', 'duelist',    'Duelist',    1),
    ('role', 'initiator',  'Initiator',  2),
    ('role', 'controller', 'Controller', 3),
    ('role', 'sentinel',   'Sentinel',   4),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'premier',            'Premier',                5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'valorant';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('riot', 'tracker.gg', 'https://tracker.gg/valorant/profile/riot/')
) as tracker (contact_kind, title, template)
where game.handle = 'valorant';
