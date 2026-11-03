-- Delta Force
--
-- Mode says which of the game's games a post is after, and LFG channels are split
-- by it: Operations is the extraction mode, Warfare the large-scale one,
-- Detonation the 5v5 bomb mode, and Campaign the co-op missions, Black Hawk Down
-- among them. Hot Zone is part of Operations, as the game's notes list it.
--
-- Operations and Warfare each keep a ladder of their own, with different names,
-- so a post fills the rank of the mode it plays and both lead the card. Ranks
-- carry their divisions, in the Roman numerals the game writes them in, so a
-- rank range spans divisions and rank closeness counts in steps of one (brief
-- 7.2). Divisions count down, so the ordinals run Bronze III, Bronze II, Bronze I,
-- Silver III and on: worst to best, the reverse of the numbering inside each
-- tier. The two ladders share their shape: three divisions in the two lowest
-- tiers, four in the next two, five in the next two, and none at the top. DF
-- Pinnacle is one step however many stars a player has earned in it.
-- Detonation has a ladder too, which is not asked: the mode is in beta and Team
-- Jade has not published the ladder.
--
-- Difficulty is how hard an Operations map a post wants to run, which those posts
-- give more often than a rank ("lf2 zd norms"). It is asked about Operations
-- alone and only of players and groups.
--
-- There is no role field: LFG posts name a map and a difficulty, and hardly ever
-- a class, though the game has four. Players name no shotcaller either.
--
-- Looking for has no team format. The Global League's qualifiers and the
-- partner teams' RISE Series are not recruited for by name, so they fall under
-- Scrims and tournaments.
--
-- Delta Force runs on PC, PlayStation, Xbox and mobile. Console plays apart from
-- PC unless a player turns crossplay on, and mobile plays apart from both, so
-- what a post plays on is worth a field here.
--
-- Players add each other in the game's Social screen on every platform, so the
-- Delta Force ID, the number on a player's profile, is the account they exchange.
-- Console players give their PSN ID or gamertag too. Steam and Epic are not
-- offered: posts don't name them. rnkd.gg's profile address also carries the
-- player's name, so its tracker is its search, which takes the ID alone and
-- redirects to the profile.

insert into game (title, short_title, handle, description)
values
    ( 'Delta Force'
    , 'Delta Force'
    , 'delta-force'
    , array['Find Delta Force players, groups and communities: a squad for Operations, a team for Warfare or Detonation, or a clan to join.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('delta_force'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'delta-force';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('mode',            'Mode',            'multi',  false, false, array['player', 'group', 'community'], true,  1),
    ('operations-rank', 'Operations rank', 'single', true,  false, array['player', 'group'],              true,  2),
    ('warfare-rank',    'Warfare rank',    'single', true,  false, array['player', 'group'],              true,  3),
    ('difficulty',      'Difficulty',      'multi',  false, false, array['player', 'group'],              true,  4),
    ('platform',        'Platform',        'multi',  false, false, array['player', 'group', 'community'], true,  5),
    ('looking-for',     'Looking for',     'multi',  false, false, array['player', 'group', 'community'], true,  6)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'delta-force';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('mode', 'operations', 'Operations', 1),
    ('mode', 'warfare',    'Warfare',    2),
    ('mode', 'detonation', 'Detonation', 3),
    ('mode', 'campaign',   'Campaign',   4),

    ('operations-rank', 'bronze-iii',     'Bronze III',     1),
    ('operations-rank', 'bronze-ii',      'Bronze II',      2),
    ('operations-rank', 'bronze-i',       'Bronze I',       3),
    ('operations-rank', 'silver-iii',     'Silver III',     4),
    ('operations-rank', 'silver-ii',      'Silver II',      5),
    ('operations-rank', 'silver-i',       'Silver I',       6),
    ('operations-rank', 'gold-iv',        'Gold IV',        7),
    ('operations-rank', 'gold-iii',       'Gold III',       8),
    ('operations-rank', 'gold-ii',        'Gold II',        9),
    ('operations-rank', 'gold-i',         'Gold I',         10),
    ('operations-rank', 'platinum-iv',    'Platinum IV',    11),
    ('operations-rank', 'platinum-iii',   'Platinum III',   12),
    ('operations-rank', 'platinum-ii',    'Platinum II',    13),
    ('operations-rank', 'platinum-i',     'Platinum I',     14),
    ('operations-rank', 'diamond-v',      'Diamond V',      15),
    ('operations-rank', 'diamond-iv',     'Diamond IV',     16),
    ('operations-rank', 'diamond-iii',    'Diamond III',    17),
    ('operations-rank', 'diamond-ii',     'Diamond II',     18),
    ('operations-rank', 'diamond-i',      'Diamond I',      19),
    ('operations-rank', 'black-hawk-v',   'Black Hawk V',   20),
    ('operations-rank', 'black-hawk-iv',  'Black Hawk IV',  21),
    ('operations-rank', 'black-hawk-iii', 'Black Hawk III', 22),
    ('operations-rank', 'black-hawk-ii',  'Black Hawk II',  23),
    ('operations-rank', 'black-hawk-i',   'Black Hawk I',   24),
    ('operations-rank', 'df-pinnacle',    'DF Pinnacle',    25),

    ('warfare-rank', 'private-iii',    'Private III',    1),
    ('warfare-rank', 'private-ii',     'Private II',     2),
    ('warfare-rank', 'private-i',      'Private I',      3),
    ('warfare-rank', 'corporal-iii',   'Corporal III',   4),
    ('warfare-rank', 'corporal-ii',    'Corporal II',    5),
    ('warfare-rank', 'corporal-i',     'Corporal I',     6),
    ('warfare-rank', 'sergeant-iv',    'Sergeant IV',    7),
    ('warfare-rank', 'sergeant-iii',   'Sergeant III',   8),
    ('warfare-rank', 'sergeant-ii',    'Sergeant II',    9),
    ('warfare-rank', 'sergeant-i',     'Sergeant I',     10),
    ('warfare-rank', 'lieutenant-iv',  'Lieutenant IV',  11),
    ('warfare-rank', 'lieutenant-iii', 'Lieutenant III', 12),
    ('warfare-rank', 'lieutenant-ii',  'Lieutenant II',  13),
    ('warfare-rank', 'lieutenant-i',   'Lieutenant I',   14),
    ('warfare-rank', 'colonel-v',      'Colonel V',      15),
    ('warfare-rank', 'colonel-iv',     'Colonel IV',     16),
    ('warfare-rank', 'colonel-iii',    'Colonel III',    17),
    ('warfare-rank', 'colonel-ii',     'Colonel II',     18),
    ('warfare-rank', 'colonel-i',      'Colonel I',      19),
    ('warfare-rank', 'general-v',      'General V',      20),
    ('warfare-rank', 'general-iv',     'General IV',     21),
    ('warfare-rank', 'general-iii',    'General III',    22),
    ('warfare-rank', 'general-ii',     'General II',     23),
    ('warfare-rank', 'general-i',      'General I',      24),
    ('warfare-rank', 'marshal',        'Marshal',        25),

    ('difficulty', 'easy',   'Easy',   1),
    ('difficulty', 'normal', 'Normal', 2),
    ('difficulty', 'hard',   'Hard',   3),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),
    ('platform', 'mobile',      'Mobile',      4),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'delta-force';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('delta_force', 'rnkd.gg', 'https://rnkd.gg/deltaforce/player/search/?q=')
) as tracker (contact_kind, title, template)
where game.handle = 'delta-force';
