-- Rocket League
--
-- Rank is per playlist: every competitive playlist keeps its own, and a player
-- states the ranks of the ones they play ("2s Diamond III, 3s Diamond II"), so
-- the game has a rank field per playlist rather than one. 2s and 3s are where
-- players look for teammates, so both lead the card, and a post that plays one
-- fills one. 1s and the five extra modes, Hoops, Rumble, Dropshot, Snow Day and
-- Heatseeker, are ladders of their own behind Details. The fields say 2s, 3s
-- and 1s, as players do, not Doubles, Standard and Duel.
--
-- Ranks run Bronze I to Grand Champion III, as the game writes them, and
-- Supersonic Legend has no numeral. Each rank splits into four divisions, but
-- players give the rank ("C2", "Diamond 3") and seldom the division under it,
-- so the options stop at the rank, a range reads "Diamond II – Champion I", and
-- rank closeness counts in steps of one rank (brief 7.2).
--
-- There is no role field: the three players of a team rotate through the same
-- positions, and posts ask for someone who rotates, never for a slot. Nobody
-- names a shotcaller either.
--
-- Server is which of the game's servers a post plays on, not where its players
-- live: that is the post's own regions field. North American posts name East or
-- West, which the North America region can't say.
--
-- RLCS is the one team format in Looking for: the open qualifiers anyone can
-- enter, with a roster formed ahead of time and recruited for by name. The
-- brackets the game schedules itself have no name but "tournaments", so they
-- are Scrims and tournaments, which is what posts recruiting for them say.
--
-- Rocket League runs on PC, PlayStation, Xbox and Switch, with crossplay
-- everywhere, ranked included, unless a player turns it off. What a post plays
-- on decides which account it offers and whether a party can use its console's
-- own chat, so it is still a field here.
--
-- The friend list is Epic's on every platform, so players add each other by
-- Epic ID, and console players by their PSN ID, gamertag or friend code too.
-- Steam is not offered: the game is unlisted there, and its Steam players add
-- each other through the same Epic friend list.

insert into game (title, short_title, handle, description)
values
    ( 'Rocket League'
    , 'Rocket League'
    , 'rocket-league'
    , array['Find Rocket League players, groups and communities: a partner for 2s, a third for 3s, a team for tournaments or RLCS, or a club to join.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('epic'), ('psn'), ('gamer_tag'), ('friend_code')) as contact (kind)
where game.handle = 'rocket-league';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('2s-rank',         '2s rank',         'single', true,  false, array['player', 'group'],              true,  1),
    ('3s-rank',         '3s rank',         'single', true,  false, array['player', 'group'],              true,  2),
    ('1s-rank',         '1s rank',         'single', true,  false, array['player', 'group'],              false, 3),
    ('hoops-rank',      'Hoops rank',      'single', true,  false, array['player', 'group'],              false, 4),
    ('rumble-rank',     'Rumble rank',     'single', true,  false, array['player', 'group'],              false, 5),
    ('dropshot-rank',   'Dropshot rank',   'single', true,  false, array['player', 'group'],              false, 6),
    ('snow-day-rank',   'Snow Day rank',   'single', true,  false, array['player', 'group'],              false, 7),
    ('heatseeker-rank', 'Heatseeker rank', 'single', true,  false, array['player', 'group'],              false, 8),
    ('server',          'Server',          'multi',  false, false, array['player', 'group', 'community'], false, 9),
    ('platform',        'Platform',        'multi',  false, false, array['player', 'group', 'community'], true,  10),
    ('looking-for',     'Looking for',     'multi',  false, false, array['player', 'group', 'community'], true,  11)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'rocket-league';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('2s-rank',         'bronze-i',           'Bronze I',           1),
    ('2s-rank',         'bronze-ii',          'Bronze II',          2),
    ('2s-rank',         'bronze-iii',         'Bronze III',         3),
    ('2s-rank',         'silver-i',           'Silver I',           4),
    ('2s-rank',         'silver-ii',          'Silver II',          5),
    ('2s-rank',         'silver-iii',         'Silver III',         6),
    ('2s-rank',         'gold-i',             'Gold I',             7),
    ('2s-rank',         'gold-ii',            'Gold II',            8),
    ('2s-rank',         'gold-iii',           'Gold III',           9),
    ('2s-rank',         'platinum-i',         'Platinum I',         10),
    ('2s-rank',         'platinum-ii',        'Platinum II',        11),
    ('2s-rank',         'platinum-iii',       'Platinum III',       12),
    ('2s-rank',         'diamond-i',          'Diamond I',          13),
    ('2s-rank',         'diamond-ii',         'Diamond II',         14),
    ('2s-rank',         'diamond-iii',        'Diamond III',        15),
    ('2s-rank',         'champion-i',         'Champion I',         16),
    ('2s-rank',         'champion-ii',        'Champion II',        17),
    ('2s-rank',         'champion-iii',       'Champion III',       18),
    ('2s-rank',         'grand-champion-i',   'Grand Champion I',   19),
    ('2s-rank',         'grand-champion-ii',  'Grand Champion II',  20),
    ('2s-rank',         'grand-champion-iii', 'Grand Champion III', 21),
    ('2s-rank',         'supersonic-legend',  'Supersonic Legend',  22),

    ('3s-rank',         'bronze-i',           'Bronze I',           1),
    ('3s-rank',         'bronze-ii',          'Bronze II',          2),
    ('3s-rank',         'bronze-iii',         'Bronze III',         3),
    ('3s-rank',         'silver-i',           'Silver I',           4),
    ('3s-rank',         'silver-ii',          'Silver II',          5),
    ('3s-rank',         'silver-iii',         'Silver III',         6),
    ('3s-rank',         'gold-i',             'Gold I',             7),
    ('3s-rank',         'gold-ii',            'Gold II',            8),
    ('3s-rank',         'gold-iii',           'Gold III',           9),
    ('3s-rank',         'platinum-i',         'Platinum I',         10),
    ('3s-rank',         'platinum-ii',        'Platinum II',        11),
    ('3s-rank',         'platinum-iii',       'Platinum III',       12),
    ('3s-rank',         'diamond-i',          'Diamond I',          13),
    ('3s-rank',         'diamond-ii',         'Diamond II',         14),
    ('3s-rank',         'diamond-iii',        'Diamond III',        15),
    ('3s-rank',         'champion-i',         'Champion I',         16),
    ('3s-rank',         'champion-ii',        'Champion II',        17),
    ('3s-rank',         'champion-iii',       'Champion III',       18),
    ('3s-rank',         'grand-champion-i',   'Grand Champion I',   19),
    ('3s-rank',         'grand-champion-ii',  'Grand Champion II',  20),
    ('3s-rank',         'grand-champion-iii', 'Grand Champion III', 21),
    ('3s-rank',         'supersonic-legend',  'Supersonic Legend',  22),

    ('1s-rank',         'bronze-i',           'Bronze I',           1),
    ('1s-rank',         'bronze-ii',          'Bronze II',          2),
    ('1s-rank',         'bronze-iii',         'Bronze III',         3),
    ('1s-rank',         'silver-i',           'Silver I',           4),
    ('1s-rank',         'silver-ii',          'Silver II',          5),
    ('1s-rank',         'silver-iii',         'Silver III',         6),
    ('1s-rank',         'gold-i',             'Gold I',             7),
    ('1s-rank',         'gold-ii',            'Gold II',            8),
    ('1s-rank',         'gold-iii',           'Gold III',           9),
    ('1s-rank',         'platinum-i',         'Platinum I',         10),
    ('1s-rank',         'platinum-ii',        'Platinum II',        11),
    ('1s-rank',         'platinum-iii',       'Platinum III',       12),
    ('1s-rank',         'diamond-i',          'Diamond I',          13),
    ('1s-rank',         'diamond-ii',         'Diamond II',         14),
    ('1s-rank',         'diamond-iii',        'Diamond III',        15),
    ('1s-rank',         'champion-i',         'Champion I',         16),
    ('1s-rank',         'champion-ii',        'Champion II',        17),
    ('1s-rank',         'champion-iii',       'Champion III',       18),
    ('1s-rank',         'grand-champion-i',   'Grand Champion I',   19),
    ('1s-rank',         'grand-champion-ii',  'Grand Champion II',  20),
    ('1s-rank',         'grand-champion-iii', 'Grand Champion III', 21),
    ('1s-rank',         'supersonic-legend',  'Supersonic Legend',  22),

    ('hoops-rank',      'bronze-i',           'Bronze I',           1),
    ('hoops-rank',      'bronze-ii',          'Bronze II',          2),
    ('hoops-rank',      'bronze-iii',         'Bronze III',         3),
    ('hoops-rank',      'silver-i',           'Silver I',           4),
    ('hoops-rank',      'silver-ii',          'Silver II',          5),
    ('hoops-rank',      'silver-iii',         'Silver III',         6),
    ('hoops-rank',      'gold-i',             'Gold I',             7),
    ('hoops-rank',      'gold-ii',            'Gold II',            8),
    ('hoops-rank',      'gold-iii',           'Gold III',           9),
    ('hoops-rank',      'platinum-i',         'Platinum I',         10),
    ('hoops-rank',      'platinum-ii',        'Platinum II',        11),
    ('hoops-rank',      'platinum-iii',       'Platinum III',       12),
    ('hoops-rank',      'diamond-i',          'Diamond I',          13),
    ('hoops-rank',      'diamond-ii',         'Diamond II',         14),
    ('hoops-rank',      'diamond-iii',        'Diamond III',        15),
    ('hoops-rank',      'champion-i',         'Champion I',         16),
    ('hoops-rank',      'champion-ii',        'Champion II',        17),
    ('hoops-rank',      'champion-iii',       'Champion III',       18),
    ('hoops-rank',      'grand-champion-i',   'Grand Champion I',   19),
    ('hoops-rank',      'grand-champion-ii',  'Grand Champion II',  20),
    ('hoops-rank',      'grand-champion-iii', 'Grand Champion III', 21),
    ('hoops-rank',      'supersonic-legend',  'Supersonic Legend',  22),

    ('rumble-rank',     'bronze-i',           'Bronze I',           1),
    ('rumble-rank',     'bronze-ii',          'Bronze II',          2),
    ('rumble-rank',     'bronze-iii',         'Bronze III',         3),
    ('rumble-rank',     'silver-i',           'Silver I',           4),
    ('rumble-rank',     'silver-ii',          'Silver II',          5),
    ('rumble-rank',     'silver-iii',         'Silver III',         6),
    ('rumble-rank',     'gold-i',             'Gold I',             7),
    ('rumble-rank',     'gold-ii',            'Gold II',            8),
    ('rumble-rank',     'gold-iii',           'Gold III',           9),
    ('rumble-rank',     'platinum-i',         'Platinum I',         10),
    ('rumble-rank',     'platinum-ii',        'Platinum II',        11),
    ('rumble-rank',     'platinum-iii',       'Platinum III',       12),
    ('rumble-rank',     'diamond-i',          'Diamond I',          13),
    ('rumble-rank',     'diamond-ii',         'Diamond II',         14),
    ('rumble-rank',     'diamond-iii',        'Diamond III',        15),
    ('rumble-rank',     'champion-i',         'Champion I',         16),
    ('rumble-rank',     'champion-ii',        'Champion II',        17),
    ('rumble-rank',     'champion-iii',       'Champion III',       18),
    ('rumble-rank',     'grand-champion-i',   'Grand Champion I',   19),
    ('rumble-rank',     'grand-champion-ii',  'Grand Champion II',  20),
    ('rumble-rank',     'grand-champion-iii', 'Grand Champion III', 21),
    ('rumble-rank',     'supersonic-legend',  'Supersonic Legend',  22),

    ('dropshot-rank',   'bronze-i',           'Bronze I',           1),
    ('dropshot-rank',   'bronze-ii',          'Bronze II',          2),
    ('dropshot-rank',   'bronze-iii',         'Bronze III',         3),
    ('dropshot-rank',   'silver-i',           'Silver I',           4),
    ('dropshot-rank',   'silver-ii',          'Silver II',          5),
    ('dropshot-rank',   'silver-iii',         'Silver III',         6),
    ('dropshot-rank',   'gold-i',             'Gold I',             7),
    ('dropshot-rank',   'gold-ii',            'Gold II',            8),
    ('dropshot-rank',   'gold-iii',           'Gold III',           9),
    ('dropshot-rank',   'platinum-i',         'Platinum I',         10),
    ('dropshot-rank',   'platinum-ii',        'Platinum II',        11),
    ('dropshot-rank',   'platinum-iii',       'Platinum III',       12),
    ('dropshot-rank',   'diamond-i',          'Diamond I',          13),
    ('dropshot-rank',   'diamond-ii',         'Diamond II',         14),
    ('dropshot-rank',   'diamond-iii',        'Diamond III',        15),
    ('dropshot-rank',   'champion-i',         'Champion I',         16),
    ('dropshot-rank',   'champion-ii',        'Champion II',        17),
    ('dropshot-rank',   'champion-iii',       'Champion III',       18),
    ('dropshot-rank',   'grand-champion-i',   'Grand Champion I',   19),
    ('dropshot-rank',   'grand-champion-ii',  'Grand Champion II',  20),
    ('dropshot-rank',   'grand-champion-iii', 'Grand Champion III', 21),
    ('dropshot-rank',   'supersonic-legend',  'Supersonic Legend',  22),

    ('snow-day-rank',   'bronze-i',           'Bronze I',           1),
    ('snow-day-rank',   'bronze-ii',          'Bronze II',          2),
    ('snow-day-rank',   'bronze-iii',         'Bronze III',         3),
    ('snow-day-rank',   'silver-i',           'Silver I',           4),
    ('snow-day-rank',   'silver-ii',          'Silver II',          5),
    ('snow-day-rank',   'silver-iii',         'Silver III',         6),
    ('snow-day-rank',   'gold-i',             'Gold I',             7),
    ('snow-day-rank',   'gold-ii',            'Gold II',            8),
    ('snow-day-rank',   'gold-iii',           'Gold III',           9),
    ('snow-day-rank',   'platinum-i',         'Platinum I',         10),
    ('snow-day-rank',   'platinum-ii',        'Platinum II',        11),
    ('snow-day-rank',   'platinum-iii',       'Platinum III',       12),
    ('snow-day-rank',   'diamond-i',          'Diamond I',          13),
    ('snow-day-rank',   'diamond-ii',         'Diamond II',         14),
    ('snow-day-rank',   'diamond-iii',        'Diamond III',        15),
    ('snow-day-rank',   'champion-i',         'Champion I',         16),
    ('snow-day-rank',   'champion-ii',        'Champion II',        17),
    ('snow-day-rank',   'champion-iii',       'Champion III',       18),
    ('snow-day-rank',   'grand-champion-i',   'Grand Champion I',   19),
    ('snow-day-rank',   'grand-champion-ii',  'Grand Champion II',  20),
    ('snow-day-rank',   'grand-champion-iii', 'Grand Champion III', 21),
    ('snow-day-rank',   'supersonic-legend',  'Supersonic Legend',  22),

    ('heatseeker-rank', 'bronze-i',           'Bronze I',           1),
    ('heatseeker-rank', 'bronze-ii',          'Bronze II',          2),
    ('heatseeker-rank', 'bronze-iii',         'Bronze III',         3),
    ('heatseeker-rank', 'silver-i',           'Silver I',           4),
    ('heatseeker-rank', 'silver-ii',          'Silver II',          5),
    ('heatseeker-rank', 'silver-iii',         'Silver III',         6),
    ('heatseeker-rank', 'gold-i',             'Gold I',             7),
    ('heatseeker-rank', 'gold-ii',            'Gold II',            8),
    ('heatseeker-rank', 'gold-iii',           'Gold III',           9),
    ('heatseeker-rank', 'platinum-i',         'Platinum I',         10),
    ('heatseeker-rank', 'platinum-ii',        'Platinum II',        11),
    ('heatseeker-rank', 'platinum-iii',       'Platinum III',       12),
    ('heatseeker-rank', 'diamond-i',          'Diamond I',          13),
    ('heatseeker-rank', 'diamond-ii',         'Diamond II',         14),
    ('heatseeker-rank', 'diamond-iii',        'Diamond III',        15),
    ('heatseeker-rank', 'champion-i',         'Champion I',         16),
    ('heatseeker-rank', 'champion-ii',        'Champion II',        17),
    ('heatseeker-rank', 'champion-iii',       'Champion III',       18),
    ('heatseeker-rank', 'grand-champion-i',   'Grand Champion I',   19),
    ('heatseeker-rank', 'grand-champion-ii',  'Grand Champion II',  20),
    ('heatseeker-rank', 'grand-champion-iii', 'Grand Champion III', 21),
    ('heatseeker-rank', 'supersonic-legend',  'Supersonic Legend',  22),

    ('server', 'us-west',          'US-West',          1),
    ('server', 'us-central',       'US-Central',       2),
    ('server', 'us-east',          'US-East',          3),
    ('server', 'south-america',    'South America',    4),
    ('server', 'europe',           'Europe',           5),
    ('server', 'middle-east',      'Middle-East',      6),
    ('server', 'south-africa',     'South Africa',     7),
    ('server', 'india',            'India',            8),
    ('server', 'asia-se-mainland', 'Asia-SE Mainland', 9),
    ('server', 'asia-se-maritime', 'Asia-SE Maritime', 10),
    ('server', 'asia-east',        'Asia-East',        11),
    ('server', 'oceania',          'Oceania',          12),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),
    ('platform', 'switch',      'Switch',      4),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'rlcs',               'RLCS',                   5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'rocket-league';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('epic',      'rocketleague.tracker.network', 'https://rocketleague.tracker.network/rocket-league/profile/epic/'),
    ('psn',       'rocketleague.tracker.network', 'https://rocketleague.tracker.network/rocket-league/profile/psn/'),
    ('gamer_tag', 'rocketleague.tracker.network', 'https://rocketleague.tracker.network/rocket-league/profile/xbl/')
) as tracker (contact_kind, title, template)
where game.handle = 'rocket-league';
