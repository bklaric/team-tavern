-- Overwatch
--
-- Rank is per role: the role queue tracks Tank, DPS and Support separately, and
-- a player states the ranks of the roles they play, so the game has three rank
-- fields rather than one. None of them is the primary one and each answers for
-- a role the post already names, so all three lead the card; a post that plays
-- one role fills one of them and leaves the others empty. 6v6 Open Queue keeps
-- its own single rank, which players give beside their role ranks ("Emerald
-- 6v6"), so it is a fourth ladder behind Details.
--
-- Ranks carry their divisions, 5 lowest to 1 highest, the way players write
-- them ("Emerald 3", "Champ 2"), so a rank range spans divisions and rank
-- closeness counts in steps of one (brief 7.2). Every tier has five, Champion
-- included; Top 500 is a leaderboard, not a tier.
--
-- Roles are the three the queue asks for, labelled as players write them: DPS,
-- not Damage. They are slotted: two players fit by covering two different ones.
--
-- Overwatch runs on PC, PlayStation, Xbox and Switch. Crossplay is everywhere
-- outside competitive, where the consoles share a pool and one PC player pulls
-- the whole group into the PC one, so what a post plays on is worth a field here.

insert into game (title, short_title, handle, description)
values
    ( 'Overwatch'
    , 'Overwatch'
    , 'overwatch'
    , array['Find Overwatch players, groups and communities: a duo for quick play, a stack to climb ranked with, or a team for scrims and tournaments.']
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
    ('tank-rank',    'Tank rank',    'single', true,  false, array['player', 'group'],              true,  1),
    ('dps-rank',     'DPS rank',     'single', true,  false, array['player', 'group'],              true,  2),
    ('support-rank', 'Support rank', 'single', true,  false, array['player', 'group'],              true,  3),
    ('6v6-rank',     '6v6 rank',     'single', true,  false, array['player', 'group'],              false, 4),
    ('role',         'Role',         'multi',  false, true,  array['player', 'group'],              true,  5),
    ('platform',     'Platform',     'multi',  false, false, array['player', 'group', 'community'], true,  6),
    ('looking-for',  'Looking for',  'multi',  false, false, array['player', 'group', 'community'], true,  7)
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
    ('tank-rank',    'emerald-5',          'Emerald 5',              21),
    ('tank-rank',    'emerald-4',          'Emerald 4',              22),
    ('tank-rank',    'emerald-3',          'Emerald 3',              23),
    ('tank-rank',    'emerald-2',          'Emerald 2',              24),
    ('tank-rank',    'emerald-1',          'Emerald 1',              25),
    ('tank-rank',    'diamond-5',          'Diamond 5',              26),
    ('tank-rank',    'diamond-4',          'Diamond 4',              27),
    ('tank-rank',    'diamond-3',          'Diamond 3',              28),
    ('tank-rank',    'diamond-2',          'Diamond 2',              29),
    ('tank-rank',    'diamond-1',          'Diamond 1',              30),
    ('tank-rank',    'master-5',           'Master 5',               31),
    ('tank-rank',    'master-4',           'Master 4',               32),
    ('tank-rank',    'master-3',           'Master 3',               33),
    ('tank-rank',    'master-2',           'Master 2',               34),
    ('tank-rank',    'master-1',           'Master 1',               35),
    ('tank-rank',    'grandmaster-5',      'Grandmaster 5',          36),
    ('tank-rank',    'grandmaster-4',      'Grandmaster 4',          37),
    ('tank-rank',    'grandmaster-3',      'Grandmaster 3',          38),
    ('tank-rank',    'grandmaster-2',      'Grandmaster 2',          39),
    ('tank-rank',    'grandmaster-1',      'Grandmaster 1',          40),
    ('tank-rank',    'champion-5',         'Champion 5',             41),
    ('tank-rank',    'champion-4',         'Champion 4',             42),
    ('tank-rank',    'champion-3',         'Champion 3',             43),
    ('tank-rank',    'champion-2',         'Champion 2',             44),
    ('tank-rank',    'champion-1',         'Champion 1',             45),

    ('dps-rank',     'bronze-5',           'Bronze 5',               1),
    ('dps-rank',     'bronze-4',           'Bronze 4',               2),
    ('dps-rank',     'bronze-3',           'Bronze 3',               3),
    ('dps-rank',     'bronze-2',           'Bronze 2',               4),
    ('dps-rank',     'bronze-1',           'Bronze 1',               5),
    ('dps-rank',     'silver-5',           'Silver 5',               6),
    ('dps-rank',     'silver-4',           'Silver 4',               7),
    ('dps-rank',     'silver-3',           'Silver 3',               8),
    ('dps-rank',     'silver-2',           'Silver 2',               9),
    ('dps-rank',     'silver-1',           'Silver 1',               10),
    ('dps-rank',     'gold-5',             'Gold 5',                 11),
    ('dps-rank',     'gold-4',             'Gold 4',                 12),
    ('dps-rank',     'gold-3',             'Gold 3',                 13),
    ('dps-rank',     'gold-2',             'Gold 2',                 14),
    ('dps-rank',     'gold-1',             'Gold 1',                 15),
    ('dps-rank',     'platinum-5',         'Platinum 5',             16),
    ('dps-rank',     'platinum-4',         'Platinum 4',             17),
    ('dps-rank',     'platinum-3',         'Platinum 3',             18),
    ('dps-rank',     'platinum-2',         'Platinum 2',             19),
    ('dps-rank',     'platinum-1',         'Platinum 1',             20),
    ('dps-rank',     'emerald-5',          'Emerald 5',              21),
    ('dps-rank',     'emerald-4',          'Emerald 4',              22),
    ('dps-rank',     'emerald-3',          'Emerald 3',              23),
    ('dps-rank',     'emerald-2',          'Emerald 2',              24),
    ('dps-rank',     'emerald-1',          'Emerald 1',              25),
    ('dps-rank',     'diamond-5',          'Diamond 5',              26),
    ('dps-rank',     'diamond-4',          'Diamond 4',              27),
    ('dps-rank',     'diamond-3',          'Diamond 3',              28),
    ('dps-rank',     'diamond-2',          'Diamond 2',              29),
    ('dps-rank',     'diamond-1',          'Diamond 1',              30),
    ('dps-rank',     'master-5',           'Master 5',               31),
    ('dps-rank',     'master-4',           'Master 4',               32),
    ('dps-rank',     'master-3',           'Master 3',               33),
    ('dps-rank',     'master-2',           'Master 2',               34),
    ('dps-rank',     'master-1',           'Master 1',               35),
    ('dps-rank',     'grandmaster-5',      'Grandmaster 5',          36),
    ('dps-rank',     'grandmaster-4',      'Grandmaster 4',          37),
    ('dps-rank',     'grandmaster-3',      'Grandmaster 3',          38),
    ('dps-rank',     'grandmaster-2',      'Grandmaster 2',          39),
    ('dps-rank',     'grandmaster-1',      'Grandmaster 1',          40),
    ('dps-rank',     'champion-5',         'Champion 5',             41),
    ('dps-rank',     'champion-4',         'Champion 4',             42),
    ('dps-rank',     'champion-3',         'Champion 3',             43),
    ('dps-rank',     'champion-2',         'Champion 2',             44),
    ('dps-rank',     'champion-1',         'Champion 1',             45),

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
    ('support-rank', 'emerald-5',          'Emerald 5',              21),
    ('support-rank', 'emerald-4',          'Emerald 4',              22),
    ('support-rank', 'emerald-3',          'Emerald 3',              23),
    ('support-rank', 'emerald-2',          'Emerald 2',              24),
    ('support-rank', 'emerald-1',          'Emerald 1',              25),
    ('support-rank', 'diamond-5',          'Diamond 5',              26),
    ('support-rank', 'diamond-4',          'Diamond 4',              27),
    ('support-rank', 'diamond-3',          'Diamond 3',              28),
    ('support-rank', 'diamond-2',          'Diamond 2',              29),
    ('support-rank', 'diamond-1',          'Diamond 1',              30),
    ('support-rank', 'master-5',           'Master 5',               31),
    ('support-rank', 'master-4',           'Master 4',               32),
    ('support-rank', 'master-3',           'Master 3',               33),
    ('support-rank', 'master-2',           'Master 2',               34),
    ('support-rank', 'master-1',           'Master 1',               35),
    ('support-rank', 'grandmaster-5',      'Grandmaster 5',          36),
    ('support-rank', 'grandmaster-4',      'Grandmaster 4',          37),
    ('support-rank', 'grandmaster-3',      'Grandmaster 3',          38),
    ('support-rank', 'grandmaster-2',      'Grandmaster 2',          39),
    ('support-rank', 'grandmaster-1',      'Grandmaster 1',          40),
    ('support-rank', 'champion-5',         'Champion 5',             41),
    ('support-rank', 'champion-4',         'Champion 4',             42),
    ('support-rank', 'champion-3',         'Champion 3',             43),
    ('support-rank', 'champion-2',         'Champion 2',             44),
    ('support-rank', 'champion-1',         'Champion 1',             45),

    ('6v6-rank',     'bronze-5',           'Bronze 5',               1),
    ('6v6-rank',     'bronze-4',           'Bronze 4',               2),
    ('6v6-rank',     'bronze-3',           'Bronze 3',               3),
    ('6v6-rank',     'bronze-2',           'Bronze 2',               4),
    ('6v6-rank',     'bronze-1',           'Bronze 1',               5),
    ('6v6-rank',     'silver-5',           'Silver 5',               6),
    ('6v6-rank',     'silver-4',           'Silver 4',               7),
    ('6v6-rank',     'silver-3',           'Silver 3',               8),
    ('6v6-rank',     'silver-2',           'Silver 2',               9),
    ('6v6-rank',     'silver-1',           'Silver 1',               10),
    ('6v6-rank',     'gold-5',             'Gold 5',                 11),
    ('6v6-rank',     'gold-4',             'Gold 4',                 12),
    ('6v6-rank',     'gold-3',             'Gold 3',                 13),
    ('6v6-rank',     'gold-2',             'Gold 2',                 14),
    ('6v6-rank',     'gold-1',             'Gold 1',                 15),
    ('6v6-rank',     'platinum-5',         'Platinum 5',             16),
    ('6v6-rank',     'platinum-4',         'Platinum 4',             17),
    ('6v6-rank',     'platinum-3',         'Platinum 3',             18),
    ('6v6-rank',     'platinum-2',         'Platinum 2',             19),
    ('6v6-rank',     'platinum-1',         'Platinum 1',             20),
    ('6v6-rank',     'emerald-5',          'Emerald 5',              21),
    ('6v6-rank',     'emerald-4',          'Emerald 4',              22),
    ('6v6-rank',     'emerald-3',          'Emerald 3',              23),
    ('6v6-rank',     'emerald-2',          'Emerald 2',              24),
    ('6v6-rank',     'emerald-1',          'Emerald 1',              25),
    ('6v6-rank',     'diamond-5',          'Diamond 5',              26),
    ('6v6-rank',     'diamond-4',          'Diamond 4',              27),
    ('6v6-rank',     'diamond-3',          'Diamond 3',              28),
    ('6v6-rank',     'diamond-2',          'Diamond 2',              29),
    ('6v6-rank',     'diamond-1',          'Diamond 1',              30),
    ('6v6-rank',     'master-5',           'Master 5',               31),
    ('6v6-rank',     'master-4',           'Master 4',               32),
    ('6v6-rank',     'master-3',           'Master 3',               33),
    ('6v6-rank',     'master-2',           'Master 2',               34),
    ('6v6-rank',     'master-1',           'Master 1',               35),
    ('6v6-rank',     'grandmaster-5',      'Grandmaster 5',          36),
    ('6v6-rank',     'grandmaster-4',      'Grandmaster 4',          37),
    ('6v6-rank',     'grandmaster-3',      'Grandmaster 3',          38),
    ('6v6-rank',     'grandmaster-2',      'Grandmaster 2',          39),
    ('6v6-rank',     'grandmaster-1',      'Grandmaster 1',          40),
    ('6v6-rank',     'champion-5',         'Champion 5',             41),
    ('6v6-rank',     'champion-4',         'Champion 4',             42),
    ('6v6-rank',     'champion-3',         'Champion 3',             43),
    ('6v6-rank',     'champion-2',         'Champion 2',             44),
    ('6v6-rank',     'champion-1',         'Champion 1',             45),

    ('role',         'tank',               'Tank',                   1),
    ('role',         'dps',                'DPS',                    2),
    ('role',         'support',            'Support',                3),

    ('platform',     'pc',                 'PC',                     1),
    ('platform',     'playstation',        'PlayStation',            2),
    ('platform',     'xbox',               'Xbox',                   3),
    ('platform',     'switch',             'Switch',                 4),

    ('looking-for',  'casual',             'Casual',                 1),
    ('looking-for',  'ranked',             'Ranked',                 2),
    ('looking-for',  'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for',  'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'overwatch';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('battle_tag', 'tracker.gg', 'https://tracker.gg/overwatch/profile/battlenet/')
) as tracker (contact_kind, title, template)
where game.handle = 'overwatch';
