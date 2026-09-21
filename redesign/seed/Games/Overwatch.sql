-- Overwatch
--
-- Rank is per role: the competitive queue tracks Tank, Damage and Support
-- separately, and a player states the ranks of the roles they play, so the game
-- has three rank fields rather than one. None of them is the primary one and
-- each answers for a role the post already names, so all three lead the card; a
-- post that plays one role fills one of them and leaves the others empty.
--
-- Ranks carry their divisions, 5 lowest to 1 highest, as the brief's cards read
-- them (brief 5.1), so a rank range spans divisions and rank closeness counts in
-- steps of one (brief 7.2). Champion is the one tier without them.
--
-- Roles are the three the queue asks for, which is what an Overwatch player says
-- they play. They are slotted: two players fit by covering two different ones.
--
-- Overwatch runs on PC, PlayStation, Xbox and Switch. Crossplay is everywhere
-- outside competitive, where the consoles share a pool and one PC player pulls
-- the whole group into the PC one, so what a post plays on is worth a field here.

insert into game (title, short_title, handle, description)
values
    ( 'Overwatch'
    , 'Overwatch'
    , 'overwatch'
    , array['Find Overwatch players, groups and communities: a duo, a five stack, or a community to play with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('battle_tag'), ('psn'), ('gamer_tag'), ('friend_code')) as contact (kind)
where game.handle = 'overwatch';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('tank-rank',    'Tank rank',    'single', true,  false, array['player', 'group'],              true, 1),
    ('damage-rank',  'Damage rank',  'single', true,  false, array['player', 'group'],              true, 2),
    ('support-rank', 'Support rank', 'single', true,  false, array['player', 'group'],              true, 3),
    ('role',         'Role',         'multi',  false, true,  array['player', 'group'],              true, 4),
    ('platform',     'Platform',     'multi',  false, false, array['player', 'group', 'community'], true, 5),
    ('looking-for',  'Looking for',  'multi',  false, false, array['player', 'group', 'community'], true, 6)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'overwatch';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('tank-rank',    'bronze-5',           'Bronze 5',               1),
    ('tank-rank',    'bronze-4',           'Bronze 4',               2),
    ('tank-rank',    'bronze-3',           'Bronze 3',               3),
    ('tank-rank',    'bronze-2',           'Bronze 2',               4),
    ('tank-rank',    'bronze-1',           'Bronze 1',               5),
    ('tank-rank',    'silver-5',           'Silver 5',               6),
    ('tank-rank',    'silver-4',           'Silver 4',               7),
    ('tank-rank',    'silver-3',           'Silver 3',               8),
    ('tank-rank',    'silver-2',           'Silver 2',               9),
    ('tank-rank',    'silver-1',           'Silver 1',               10),
    ('tank-rank',    'gold-5',             'Gold 5',                 11),
    ('tank-rank',    'gold-4',             'Gold 4',                 12),
    ('tank-rank',    'gold-3',             'Gold 3',                 13),
    ('tank-rank',    'gold-2',             'Gold 2',                 14),
    ('tank-rank',    'gold-1',             'Gold 1',                 15),
    ('tank-rank',    'platinum-5',         'Platinum 5',             16),
    ('tank-rank',    'platinum-4',         'Platinum 4',             17),
    ('tank-rank',    'platinum-3',         'Platinum 3',             18),
    ('tank-rank',    'platinum-2',         'Platinum 2',             19),
    ('tank-rank',    'platinum-1',         'Platinum 1',             20),
    ('tank-rank',    'diamond-5',          'Diamond 5',              21),
    ('tank-rank',    'diamond-4',          'Diamond 4',              22),
    ('tank-rank',    'diamond-3',          'Diamond 3',              23),
    ('tank-rank',    'diamond-2',          'Diamond 2',              24),
    ('tank-rank',    'diamond-1',          'Diamond 1',              25),
    ('tank-rank',    'master-5',           'Master 5',               26),
    ('tank-rank',    'master-4',           'Master 4',               27),
    ('tank-rank',    'master-3',           'Master 3',               28),
    ('tank-rank',    'master-2',           'Master 2',               29),
    ('tank-rank',    'master-1',           'Master 1',               30),
    ('tank-rank',    'grandmaster-5',      'Grandmaster 5',          31),
    ('tank-rank',    'grandmaster-4',      'Grandmaster 4',          32),
    ('tank-rank',    'grandmaster-3',      'Grandmaster 3',          33),
    ('tank-rank',    'grandmaster-2',      'Grandmaster 2',          34),
    ('tank-rank',    'grandmaster-1',      'Grandmaster 1',          35),
    ('tank-rank',    'champion',           'Champion',               36),

    ('damage-rank',  'bronze-5',           'Bronze 5',               1),
    ('damage-rank',  'bronze-4',           'Bronze 4',               2),
    ('damage-rank',  'bronze-3',           'Bronze 3',               3),
    ('damage-rank',  'bronze-2',           'Bronze 2',               4),
    ('damage-rank',  'bronze-1',           'Bronze 1',               5),
    ('damage-rank',  'silver-5',           'Silver 5',               6),
    ('damage-rank',  'silver-4',           'Silver 4',               7),
    ('damage-rank',  'silver-3',           'Silver 3',               8),
    ('damage-rank',  'silver-2',           'Silver 2',               9),
    ('damage-rank',  'silver-1',           'Silver 1',               10),
    ('damage-rank',  'gold-5',             'Gold 5',                 11),
    ('damage-rank',  'gold-4',             'Gold 4',                 12),
    ('damage-rank',  'gold-3',             'Gold 3',                 13),
    ('damage-rank',  'gold-2',             'Gold 2',                 14),
    ('damage-rank',  'gold-1',             'Gold 1',                 15),
    ('damage-rank',  'platinum-5',         'Platinum 5',             16),
    ('damage-rank',  'platinum-4',         'Platinum 4',             17),
    ('damage-rank',  'platinum-3',         'Platinum 3',             18),
    ('damage-rank',  'platinum-2',         'Platinum 2',             19),
    ('damage-rank',  'platinum-1',         'Platinum 1',             20),
    ('damage-rank',  'diamond-5',          'Diamond 5',              21),
    ('damage-rank',  'diamond-4',          'Diamond 4',              22),
    ('damage-rank',  'diamond-3',          'Diamond 3',              23),
    ('damage-rank',  'diamond-2',          'Diamond 2',              24),
    ('damage-rank',  'diamond-1',          'Diamond 1',              25),
    ('damage-rank',  'master-5',           'Master 5',               26),
    ('damage-rank',  'master-4',           'Master 4',               27),
    ('damage-rank',  'master-3',           'Master 3',               28),
    ('damage-rank',  'master-2',           'Master 2',               29),
    ('damage-rank',  'master-1',           'Master 1',               30),
    ('damage-rank',  'grandmaster-5',      'Grandmaster 5',          31),
    ('damage-rank',  'grandmaster-4',      'Grandmaster 4',          32),
    ('damage-rank',  'grandmaster-3',      'Grandmaster 3',          33),
    ('damage-rank',  'grandmaster-2',      'Grandmaster 2',          34),
    ('damage-rank',  'grandmaster-1',      'Grandmaster 1',          35),
    ('damage-rank',  'champion',           'Champion',               36),

    ('support-rank', 'bronze-5',           'Bronze 5',               1),
    ('support-rank', 'bronze-4',           'Bronze 4',               2),
    ('support-rank', 'bronze-3',           'Bronze 3',               3),
    ('support-rank', 'bronze-2',           'Bronze 2',               4),
    ('support-rank', 'bronze-1',           'Bronze 1',               5),
    ('support-rank', 'silver-5',           'Silver 5',               6),
    ('support-rank', 'silver-4',           'Silver 4',               7),
    ('support-rank', 'silver-3',           'Silver 3',               8),
    ('support-rank', 'silver-2',           'Silver 2',               9),
    ('support-rank', 'silver-1',           'Silver 1',               10),
    ('support-rank', 'gold-5',             'Gold 5',                 11),
    ('support-rank', 'gold-4',             'Gold 4',                 12),
    ('support-rank', 'gold-3',             'Gold 3',                 13),
    ('support-rank', 'gold-2',             'Gold 2',                 14),
    ('support-rank', 'gold-1',             'Gold 1',                 15),
    ('support-rank', 'platinum-5',         'Platinum 5',             16),
    ('support-rank', 'platinum-4',         'Platinum 4',             17),
    ('support-rank', 'platinum-3',         'Platinum 3',             18),
    ('support-rank', 'platinum-2',         'Platinum 2',             19),
    ('support-rank', 'platinum-1',         'Platinum 1',             20),
    ('support-rank', 'diamond-5',          'Diamond 5',              21),
    ('support-rank', 'diamond-4',          'Diamond 4',              22),
    ('support-rank', 'diamond-3',          'Diamond 3',              23),
    ('support-rank', 'diamond-2',          'Diamond 2',              24),
    ('support-rank', 'diamond-1',          'Diamond 1',              25),
    ('support-rank', 'master-5',           'Master 5',               26),
    ('support-rank', 'master-4',           'Master 4',               27),
    ('support-rank', 'master-3',           'Master 3',               28),
    ('support-rank', 'master-2',           'Master 2',               29),
    ('support-rank', 'master-1',           'Master 1',               30),
    ('support-rank', 'grandmaster-5',      'Grandmaster 5',          31),
    ('support-rank', 'grandmaster-4',      'Grandmaster 4',          32),
    ('support-rank', 'grandmaster-3',      'Grandmaster 3',          33),
    ('support-rank', 'grandmaster-2',      'Grandmaster 2',          34),
    ('support-rank', 'grandmaster-1',      'Grandmaster 1',          35),
    ('support-rank', 'champion',           'Champion',               36),

    ('role',         'tank',               'Tank',                   1),
    ('role',         'damage',             'Damage',                 2),
    ('role',         'support',            'Support',                3),

    ('platform',     'pc',                 'PC',                     1),
    ('platform',     'playstation',        'PlayStation',            2),
    ('platform',     'xbox',               'Xbox',                   3),
    ('platform',     'switch',             'Switch',                 4),

    ('looking-for',  'casual',             'Casual',                 1),
    ('looking-for',  'ranked-climb',       'Ranked climb',           2),
    ('looking-for',  'stadium',            'Stadium',                3),
    ('looking-for',  'scrims-tournaments', 'Scrims and tournaments', 4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'overwatch';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('battle_tag', 'tracker.gg', 'https://tracker.gg/overwatch/profile/battlenet/')
) as tracker (contact_kind, title, template)
where game.handle = 'overwatch';
