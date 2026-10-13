-- PUBG: Battlegrounds
--
-- Rank is one ladder: ranked points are shared by duos and squads, first and
-- third person, so a player has one rank. Ranks carry their divisions, in the
-- Roman numerals the game writes them in, so a rank range spans divisions and
-- rank closeness counts in steps of one (brief 7.2). Master and Survivor have
-- none; Survivor is Master past 3,700 points within a server's top places.
--
-- ADR, a player's average damage per round, is what PUBG players give for how
-- good they are ("lf2 300+", "lfs 400") far more often than their rank, so it
-- leads the card beside it. The game keeps no ladder of it; the trackers show
-- it. The options are uniform bands fifty wide, which is how players round it,
-- each read as players give it, "300" for anything from three hundred to just
-- under three hundred and fifty. The bottom and top bands are open.
--
-- Perspective is whether a post plays first person or third. Every queue is
-- one or the other, and LFG channels are split by it, so it is a fact two posts
-- share.
--
-- There is no role field: LFG posts name no roles and no shotcaller.
--
-- Server is which of the game's regions a post plays on, not where its players
-- live: that is the post's own regions field. Players pick a server outside
-- their own region, Asia above all. Oceania plays on Southeast Asia on PC, and
-- console's Asia-Pacific region is Asia here. Kakao is the region of the
-- Korean PC service.
--
-- Looking for has no team format. The PUBG Global Series and the Nations Cup
-- are entered through regional qualifiers that LFG posts never name, so
-- they fall under Scrims and tournaments.
--
-- PUBG runs on PC, PlayStation and Xbox. PlayStation and Xbox play together
-- and PC plays apart, so what a post plays on is worth a field here.
--
-- On PC, players add each other by their PUBG nickname, which is the same on
-- Steam, Epic and Kakao and is what the trackers look up. On console the
-- nickname is the PSN ID or Xbox gamertag.

insert into game (title, short_title, handle, description)
values
    ( 'PUBG: Battlegrounds'
    , 'PUBG'
    , 'pubg-battlegrounds'
    , array['Find PUBG players, groups and communities: a duo or a squad for ranked, players by ADR for FPP or TPP, a team for scrims, or a clan to join.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('pubg'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'pubg-battlegrounds';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true,  1),
    ('adr',         'ADR',         'single', true,  false, array['player', 'group'],              true,  2),
    ('perspective', 'Perspective', 'multi',  false, false, array['player', 'group', 'community'], true,  3),
    ('server',      'Server',      'multi',  false, false, array['player', 'group', 'community'], false, 4),
    ('platform',    'Platform',    'multi',  false, false, array['player', 'group', 'community'], true,  5),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true,  6)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'pubg-battlegrounds';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'bronze-iv',    'Bronze IV',    1),
    ('rank', 'bronze-iii',   'Bronze III',   2),
    ('rank', 'bronze-ii',    'Bronze II',    3),
    ('rank', 'bronze-i',     'Bronze I',     4),
    ('rank', 'silver-iv',    'Silver IV',    5),
    ('rank', 'silver-iii',   'Silver III',   6),
    ('rank', 'silver-ii',    'Silver II',    7),
    ('rank', 'silver-i',     'Silver I',     8),
    ('rank', 'gold-iv',      'Gold IV',      9),
    ('rank', 'gold-iii',     'Gold III',     10),
    ('rank', 'gold-ii',      'Gold II',      11),
    ('rank', 'gold-i',       'Gold I',       12),
    ('rank', 'platinum-iv',  'Platinum IV',  13),
    ('rank', 'platinum-iii', 'Platinum III', 14),
    ('rank', 'platinum-ii',  'Platinum II',  15),
    ('rank', 'platinum-i',   'Platinum I',   16),
    ('rank', 'crystal-iv',   'Crystal IV',   17),
    ('rank', 'crystal-iii',  'Crystal III',  18),
    ('rank', 'crystal-ii',   'Crystal II',   19),
    ('rank', 'crystal-i',    'Crystal I',    20),
    ('rank', 'diamond-iv',   'Diamond IV',   21),
    ('rank', 'diamond-iii',  'Diamond III',  22),
    ('rank', 'diamond-ii',   'Diamond II',   23),
    ('rank', 'diamond-i',    'Diamond I',    24),
    ('rank', 'master',       'Master',       25),
    ('rank', 'survivor',     'Survivor',     26),

    ('adr', 'under-150', 'Under 150', 1),
    ('adr', '150',       '150',       2),
    ('adr', '200',       '200',       3),
    ('adr', '250',       '250',       4),
    ('adr', '300',       '300',       5),
    ('adr', '350',       '350',       6),
    ('adr', '400',       '400',       7),
    ('adr', '450',       '450',       8),
    ('adr', '500-plus',  '500+',      9),

    ('perspective', 'fpp', 'FPP', 1),
    ('perspective', 'tpp', 'TPP', 2),

    ('server', 'north-america',  'North America',  1),
    ('server', 'south-america',  'South America',  2),
    ('server', 'europe',         'Europe',         3),
    ('server', 'russia',         'Russia',         4),
    ('server', 'asia',           'Asia',           5),
    ('server', 'southeast-asia', 'Southeast Asia', 6),
    ('server', 'kakao',          'Kakao',          7),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'pubg-battlegrounds';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('pubg',      'op.gg',      'https://op.gg/pubg/user/'),
    ('pubg',      'tracker.gg', 'https://tracker.gg/pubg/profile/steam/'),
    ('psn',       'tracker.gg', 'https://tracker.gg/pubg/profile/psn/'),
    ('gamer_tag', 'tracker.gg', 'https://tracker.gg/pubg/profile/xbl/')
) as tracker (contact_kind, title, template)
where game.handle = 'pubg-battlegrounds';
