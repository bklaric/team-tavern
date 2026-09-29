-- Counter-Strike 2
--
-- Premier's CS Rating is a number in the thousands rather than a tier, and the
-- game only colours it, in seven bands five thousand wide. The options are
-- uniform bands of a thousand, so one step is the same distance everywhere,
-- which is what rank closeness counts in (brief 7.2); a colour band is wide
-- enough to hold the whole gap between a duo that works and one that does not.
-- Each band reads as players give their rating, "12k" for anything from twelve
-- thousand to just under thirteen. The top band is open, as the gold one is.
--
-- Competitive is the other ladder, eighteen skill groups and one of them per
-- map, so the field asks for the group a player would name themselves. It sits
-- behind Premier, which is the rating people give first. Wingman runs its 2v2
-- ladder on the same eighteen, separate from either. Danger Zone is not in CS2,
-- so nothing here ranks it.
--
-- Roles are the jobs CS players claim on LFT boards and in their own posts:
-- entry, rifler, AWPer, lurker and support, and the CT-side anchor, which
-- players name as often as lurker. A player gives their T-side and CT-side jobs
-- together, so the list carries both. They are slotted: two players fit by
-- covering two different ones, and two AWPers do not.
--
-- In-game leader is asked beside the roles because teams recruit for one by
-- name. It says a player can lead on top of the role they play, and any number
-- can, so it is not a slot: two players fit when either can lead.
--
-- Looking for has no team format. Premier is the ranked queue and Wingman a
-- mode, and the leagues players recruit for, ESEA and the like, are run by
-- third parties, so all of them fall under Ranked or Scrims and tournaments.
--
-- Faceit is where the ladder above Premier is played, and it grades on its ten
-- levels, so it stays as a third ordered field.
--
-- Counter-Strike 2 runs on PC alone, so there is no platform field.

insert into game (title, short_title, handle, description)
values
    ( 'Counter-Strike 2'
    , 'CS2'
    , 'counter-strike-2'
    , array['Find Counter-Strike 2 players, groups and communities: a Premier duo, a five stack for Faceit, an ESEA team, or a server to play on.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam')) as contact (kind)
where game.handle = 'counter-strike-2';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('premier-rating', 'Premier rating',          'single',  true,  false, array['player', 'group'],              true,  1),
    ('role',           'Role',                    'multi',   false, true,  array['player', 'group'],              true,  2),
    ('in-game-leader', 'In-game leader',          'boolean', false, false, array['player', 'group'],              false, 3),
    ('looking-for',    'Looking for',             'multi',   false, false, array['player', 'group', 'community'], true,  4),
    ('skill-group',    'Competitive skill group', 'single',  true,  false, array['player', 'group'],              false, 5),
    ('wingman-rank',   'Wingman rank',            'single',  true,  false, array['player', 'group'],              false, 6),
    ('faceit-level',   'Faceit level',            'single',  true,  false, array['player', 'group'],              false, 7)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'counter-strike-2';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('premier-rating', 'under-1k',  'Under 1k', 1),
    ('premier-rating', '1k',        '1k',       2),
    ('premier-rating', '2k',        '2k',       3),
    ('premier-rating', '3k',        '3k',       4),
    ('premier-rating', '4k',        '4k',       5),
    ('premier-rating', '5k',        '5k',       6),
    ('premier-rating', '6k',        '6k',       7),
    ('premier-rating', '7k',        '7k',       8),
    ('premier-rating', '8k',        '8k',       9),
    ('premier-rating', '9k',        '9k',       10),
    ('premier-rating', '10k',       '10k',      11),
    ('premier-rating', '11k',       '11k',      12),
    ('premier-rating', '12k',       '12k',      13),
    ('premier-rating', '13k',       '13k',      14),
    ('premier-rating', '14k',       '14k',      15),
    ('premier-rating', '15k',       '15k',      16),
    ('premier-rating', '16k',       '16k',      17),
    ('premier-rating', '17k',       '17k',      18),
    ('premier-rating', '18k',       '18k',      19),
    ('premier-rating', '19k',       '19k',      20),
    ('premier-rating', '20k',       '20k',      21),
    ('premier-rating', '21k',       '21k',      22),
    ('premier-rating', '22k',       '22k',      23),
    ('premier-rating', '23k',       '23k',      24),
    ('premier-rating', '24k',       '24k',      25),
    ('premier-rating', '25k',       '25k',      26),
    ('premier-rating', '26k',       '26k',      27),
    ('premier-rating', '27k',       '27k',      28),
    ('premier-rating', '28k',       '28k',      29),
    ('premier-rating', '29k',       '29k',      30),
    ('premier-rating', '30k-plus',  '30k+',     31),

    ('role', 'entry-fragger', 'Entry fragger', 1),
    ('role', 'rifler',        'Rifler',        2),
    ('role', 'awper',         'AWPer',         3),
    ('role', 'lurker',        'Lurker',        4),
    ('role', 'support',       'Support',       5),
    ('role', 'anchor',        'Anchor',        6),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),

    ('skill-group', 'silver-1',                      'Silver I',                      1),
    ('skill-group', 'silver-2',                      'Silver II',                     2),
    ('skill-group', 'silver-3',                      'Silver III',                    3),
    ('skill-group', 'silver-4',                      'Silver IV',                     4),
    ('skill-group', 'silver-elite',                  'Silver Elite',                  5),
    ('skill-group', 'silver-elite-master',           'Silver Elite Master',           6),
    ('skill-group', 'gold-nova-1',                   'Gold Nova I',                   7),
    ('skill-group', 'gold-nova-2',                   'Gold Nova II',                  8),
    ('skill-group', 'gold-nova-3',                   'Gold Nova III',                 9),
    ('skill-group', 'gold-nova-master',              'Gold Nova Master',              10),
    ('skill-group', 'master-guardian-1',             'Master Guardian I',             11),
    ('skill-group', 'master-guardian-2',             'Master Guardian II',            12),
    ('skill-group', 'master-guardian-elite',         'Master Guardian Elite',         13),
    ('skill-group', 'distinguished-master-guardian', 'Distinguished Master Guardian', 14),
    ('skill-group', 'legendary-eagle',               'Legendary Eagle',               15),
    ('skill-group', 'legendary-eagle-master',        'Legendary Eagle Master',        16),
    ('skill-group', 'supreme-master-first-class',    'Supreme Master First Class',    17),
    ('skill-group', 'the-global-elite',              'The Global Elite',              18),

    ('wingman-rank', 'silver-1',                      'Silver I',                      1),
    ('wingman-rank', 'silver-2',                      'Silver II',                     2),
    ('wingman-rank', 'silver-3',                      'Silver III',                    3),
    ('wingman-rank', 'silver-4',                      'Silver IV',                     4),
    ('wingman-rank', 'silver-elite',                  'Silver Elite',                  5),
    ('wingman-rank', 'silver-elite-master',           'Silver Elite Master',           6),
    ('wingman-rank', 'gold-nova-1',                   'Gold Nova I',                   7),
    ('wingman-rank', 'gold-nova-2',                   'Gold Nova II',                  8),
    ('wingman-rank', 'gold-nova-3',                   'Gold Nova III',                 9),
    ('wingman-rank', 'gold-nova-master',              'Gold Nova Master',              10),
    ('wingman-rank', 'master-guardian-1',             'Master Guardian I',             11),
    ('wingman-rank', 'master-guardian-2',             'Master Guardian II',            12),
    ('wingman-rank', 'master-guardian-elite',         'Master Guardian Elite',         13),
    ('wingman-rank', 'distinguished-master-guardian', 'Distinguished Master Guardian', 14),
    ('wingman-rank', 'legendary-eagle',               'Legendary Eagle',               15),
    ('wingman-rank', 'legendary-eagle-master',        'Legendary Eagle Master',        16),
    ('wingman-rank', 'supreme-master-first-class',    'Supreme Master First Class',    17),
    ('wingman-rank', 'the-global-elite',              'The Global Elite',              18),

    ('faceit-level', 'level-1',  'Level 1',  1),
    ('faceit-level', 'level-2',  'Level 2',  2),
    ('faceit-level', 'level-3',  'Level 3',  3),
    ('faceit-level', 'level-4',  'Level 4',  4),
    ('faceit-level', 'level-5',  'Level 5',  5),
    ('faceit-level', 'level-6',  'Level 6',  6),
    ('faceit-level', 'level-7',  'Level 7',  7),
    ('faceit-level', 'level-8',  'Level 8',  8),
    ('faceit-level', 'level-9',  'Level 9',  9),
    ('faceit-level', 'level-10', 'Level 10', 10)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'counter-strike-2';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('steam', 'tracker.gg', 'https://tracker.gg/cs2/profile/steam/'),
    ('steam', 'csstats.gg', 'https://csstats.gg/player/')
) as tracker (contact_kind, title, template)
where game.handle = 'counter-strike-2';
