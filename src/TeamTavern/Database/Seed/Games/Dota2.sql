-- Dota 2
--
-- Ranks carry their stars, "Legend 3" as players say them, so a rank range
-- spans them and rank closeness counts in steps of one (brief 7.2). Immortal is
-- the one medal without stars: past it the ladder is a regional leaderboard.
--
-- Positions are Dota's own 1 to 5, labelled with both the name and the number:
-- players write "carry" and "offlane" by name but "pos 4" and "pos 5" by
-- number. They are slotted: two players fit by covering two different ones,
-- which is what a Dota duo is short of. The Dota community doesn't speak of an
-- in-game leader, so there is no such field.
--
-- Server is which of Valve's regions a post queues on, not where its players
-- live: that is the post's own regions field. Dota asks for it in the
-- matchmaking settings and a stack has to agree on one, so it stays.
--
-- Battle Cup still runs its weekend bracket in the client, tiers 3 to 8, so the
-- tier stays as a second ordered field behind the medal, and Battle Cup is the
-- team format a stack is recruited for by name.
--
-- Dota 2 runs on PC alone, so there is no platform field.

insert into game (title, short_title, handle, description)
values
    ( 'Dota 2'
    , 'Dota 2'
    , 'dota-2'
    , array['Find Dota 2 players, groups and communities: a ranked duo, a five for Battle Cup, or a community to play with.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam')) as contact (kind)
where game.handle = 'dota-2';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',            'Rank',            'single', true,  false, array['player', 'group'],              true,  1),
    ('position',        'Position',        'multi',  false, true,  array['player', 'group'],              true,  2),
    ('server',          'Server',          'multi',  false, false, array['player', 'group', 'community'], false, 3),
    ('battle-cup-tier', 'Battle Cup tier', 'single', true,  false, array['player', 'group'],              false, 4),
    ('looking-for',     'Looking for',     'multi',  false, false, array['player', 'group', 'community'], true,  5)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'dota-2';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'herald-1',    'Herald 1',    1),
    ('rank', 'herald-2',    'Herald 2',    2),
    ('rank', 'herald-3',    'Herald 3',    3),
    ('rank', 'herald-4',    'Herald 4',    4),
    ('rank', 'herald-5',    'Herald 5',    5),
    ('rank', 'guardian-1',  'Guardian 1',  6),
    ('rank', 'guardian-2',  'Guardian 2',  7),
    ('rank', 'guardian-3',  'Guardian 3',  8),
    ('rank', 'guardian-4',  'Guardian 4',  9),
    ('rank', 'guardian-5',  'Guardian 5',  10),
    ('rank', 'crusader-1',  'Crusader 1',  11),
    ('rank', 'crusader-2',  'Crusader 2',  12),
    ('rank', 'crusader-3',  'Crusader 3',  13),
    ('rank', 'crusader-4',  'Crusader 4',  14),
    ('rank', 'crusader-5',  'Crusader 5',  15),
    ('rank', 'archon-1',    'Archon 1',    16),
    ('rank', 'archon-2',    'Archon 2',    17),
    ('rank', 'archon-3',    'Archon 3',    18),
    ('rank', 'archon-4',    'Archon 4',    19),
    ('rank', 'archon-5',    'Archon 5',    20),
    ('rank', 'legend-1',    'Legend 1',    21),
    ('rank', 'legend-2',    'Legend 2',    22),
    ('rank', 'legend-3',    'Legend 3',    23),
    ('rank', 'legend-4',    'Legend 4',    24),
    ('rank', 'legend-5',    'Legend 5',    25),
    ('rank', 'ancient-1',   'Ancient 1',   26),
    ('rank', 'ancient-2',   'Ancient 2',   27),
    ('rank', 'ancient-3',   'Ancient 3',   28),
    ('rank', 'ancient-4',   'Ancient 4',   29),
    ('rank', 'ancient-5',   'Ancient 5',   30),
    ('rank', 'divine-1',    'Divine 1',    31),
    ('rank', 'divine-2',    'Divine 2',    32),
    ('rank', 'divine-3',    'Divine 3',    33),
    ('rank', 'divine-4',    'Divine 4',    34),
    ('rank', 'divine-5',    'Divine 5',    35),
    ('rank', 'immortal',    'Immortal',    36),

    ('position', 'carry',        'Carry (1)',        1),
    ('position', 'mid',          'Mid (2)',          2),
    ('position', 'offlane',      'Offlane (3)',      3),
    ('position', 'soft-support', 'Soft support (4)', 4),
    ('position', 'hard-support', 'Hard support (5)', 5),

    ('server', 'us-west',       'US West',       1),
    ('server', 'us-east',       'US East',       2),
    ('server', 'south-america', 'South America', 3),
    ('server', 'peru',          'Peru',          4),
    ('server', 'chile',         'Chile',         5),
    ('server', 'argentina',     'Argentina',     6),
    ('server', 'europe-west',   'Europe West',   7),
    ('server', 'europe-east',   'Europe East',   8),
    ('server', 'russia',        'Russia',        9),
    ('server', 'dubai',         'Dubai',         10),
    ('server', 'south-africa',  'South Africa',  11),
    ('server', 'india',         'India',         12),
    ('server', 'se-asia',       'SE Asia',       13),
    ('server', 'japan',         'Japan',         14),
    ('server', 'australia',     'Australia',     15),
    ('server', 'china',         'China',         16),

    ('battle-cup-tier', 'tier-3', 'Tier 3', 1),
    ('battle-cup-tier', 'tier-4', 'Tier 4', 2),
    ('battle-cup-tier', 'tier-5', 'Tier 5', 3),
    ('battle-cup-tier', 'tier-6', 'Tier 6', 4),
    ('battle-cup-tier', 'tier-7', 'Tier 7', 5),
    ('battle-cup-tier', 'tier-8', 'Tier 8', 6),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4),
    ('looking-for', 'battle-cup',         'Battle Cup',             5)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'dota-2';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('steam', 'opendota.com', 'https://www.opendota.com/players/'),
    ('steam', 'dotabuff.com', 'https://www.dotabuff.com/players/')
) as tracker (contact_kind, title, template)
where game.handle = 'dota-2';
