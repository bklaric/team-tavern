-- How today's games, fields, platforms and places read in the new catalogue
-- (brief 12). import.sql reads these tables; report.sql lists every answer they
-- leave unmapped, which is how this file is reviewed.
--
-- The new seeds describe the games as they are today, so an old option maps
-- only where something in the game still means the same thing. An option with
-- no row here is dropped.

create table legacy.game_map
    ( old_handle text primary key
    , new_handle text not null
    );

-- Splitgate is not in the new catalogue, so its profiles are dropped.
insert into legacy.game_map (old_handle, new_handle) values
    ('apex', 'apex'),
    ('csgo', 'cs2'),
    ('dota2', 'dota2'),
    ('hots', 'hots'),
    ('lol', 'lol'),
    ('overwatch', 'overwatch'),
    ('r6s', 'r6s'),
    ('tf2', 'tf2'),
    ('valheim', 'valheim'),
    ('valorant', 'valorant');

-- An old option becomes a new option, or for an ordered field a span of them:
-- a tier without divisions becomes every division of the tier. import.sql turns
-- a group's spans into a range and a player's into the division in the middle
-- of it, which is within a step or two of whichever one they were in. An old
-- option may become several new ones: a Flex player plays every role. A boolean
-- field has no options, so an old option that becomes its yes names none.
--
-- A profile's new-or-returning checkbox reads as the option new-or-returning of
-- a field of the same name, so it maps like any other.
create table legacy.option_map
    ( game text not null -- the new handle
    , old_field text not null
    , old_option text not null
    , new_field text not null
    , new_from text -- null for a boolean field
    , new_to text -- the other end of the span, for an ordered field
    );

insert into legacy.option_map (game, old_field, old_option, new_field, new_from, new_to)
select game_map.new_handle, 'new-or-returning', 'new-or-returning', 'looking-for', 'learning-the-game', null
from legacy.game_map;

insert into legacy.option_map (game, old_field, old_option, new_field, new_from, new_to) values
    -- Apex. Arenas ranked is gone and nothing replaced it, so its rank drops;
    -- its queues fold into the intents they served.
    ('apex', 'battle-royale-rank', 'bronze',        'rank', 'bronze-iv',   'bronze-i'),
    ('apex', 'battle-royale-rank', 'silver',        'rank', 'silver-iv',   'silver-i'),
    ('apex', 'battle-royale-rank', 'gold',          'rank', 'gold-iv',     'gold-i'),
    ('apex', 'battle-royale-rank', 'platinum',      'rank', 'platinum-iv', 'platinum-i'),
    ('apex', 'battle-royale-rank', 'diamond',       'rank', 'diamond-iv',  'diamond-i'),
    ('apex', 'battle-royale-rank', 'master',        'rank', 'master',      null),
    ('apex', 'battle-royale-rank', 'apex-predator', 'rank', 'apex-predator', null),
    ('apex', 'interest', 'unranked-battle-royale', 'looking-for', 'casual',             null),
    ('apex', 'interest', 'unranked-arenas',        'looking-for', 'casual',             null),
    ('apex', 'interest', 'ranked-battle-royale',   'looking-for', 'ranked',             null),
    ('apex', 'interest', 'ranked-arenas',          'looking-for', 'ranked',             null),
    ('apex', 'interest', 'leagues-tournaments',    'looking-for', 'scrims-tournaments', null),

    -- Counter-Strike 2. The skill groups and Faceit levels are CS:GO's; Danger
    -- Zone is not in CS2. In-game leader is its own yes-or-no.
    ('cs2', 'competitive-rank', 'silver-i',                      'skill-group',  'silver-1',                      null),
    ('cs2', 'competitive-rank', 'silver-ii',                     'skill-group',  'silver-2',                      null),
    ('cs2', 'competitive-rank', 'silver-iii',                    'skill-group',  'silver-3',                      null),
    ('cs2', 'competitive-rank', 'silver-iv',                     'skill-group',  'silver-4',                      null),
    ('cs2', 'competitive-rank', 'silver-elite',                  'skill-group',  'silver-elite',                  null),
    ('cs2', 'competitive-rank', 'silver-elite-master',           'skill-group',  'silver-elite-master',           null),
    ('cs2', 'competitive-rank', 'gold-nova-i',                   'skill-group',  'gold-nova-1',                   null),
    ('cs2', 'competitive-rank', 'gold-nova-ii',                  'skill-group',  'gold-nova-2',                   null),
    ('cs2', 'competitive-rank', 'gold-nova-iii',                 'skill-group',  'gold-nova-3',                   null),
    ('cs2', 'competitive-rank', 'gold-nova-master',              'skill-group',  'gold-nova-master',              null),
    ('cs2', 'competitive-rank', 'master-guardian-i',             'skill-group',  'master-guardian-1',             null),
    ('cs2', 'competitive-rank', 'master-guardian-ii',            'skill-group',  'master-guardian-2',             null),
    ('cs2', 'competitive-rank', 'master-guardian-elite',         'skill-group',  'master-guardian-elite',         null),
    ('cs2', 'competitive-rank', 'distinguished-master-guardian', 'skill-group',  'distinguished-master-guardian', null),
    ('cs2', 'competitive-rank', 'legendary-eagle',               'skill-group',  'legendary-eagle',               null),
    ('cs2', 'competitive-rank', 'legendary-eagle-master',        'skill-group',  'legendary-eagle-master',        null),
    ('cs2', 'competitive-rank', 'supreme-master-first-class',    'skill-group',  'supreme-master-first-class',    null),
    ('cs2', 'competitive-rank', 'the-global-elite',              'skill-group',  'the-global-elite',              null),
    ('cs2', 'wingman-rank',     'silver-i',                      'wingman-rank', 'silver-1',                      null),
    ('cs2', 'wingman-rank',     'silver-ii',                     'wingman-rank', 'silver-2',                      null),
    ('cs2', 'wingman-rank',     'silver-iii',                    'wingman-rank', 'silver-3',                      null),
    ('cs2', 'wingman-rank',     'silver-iv',                     'wingman-rank', 'silver-4',                      null),
    ('cs2', 'wingman-rank',     'silver-elite',                  'wingman-rank', 'silver-elite',                  null),
    ('cs2', 'wingman-rank',     'silver-elite-master',           'wingman-rank', 'silver-elite-master',           null),
    ('cs2', 'wingman-rank',     'gold-nova-i',                   'wingman-rank', 'gold-nova-1',                   null),
    ('cs2', 'wingman-rank',     'gold-nova-ii',                  'wingman-rank', 'gold-nova-2',                   null),
    ('cs2', 'wingman-rank',     'gold-nova-iii',                 'wingman-rank', 'gold-nova-3',                   null),
    ('cs2', 'wingman-rank',     'gold-nova-master',              'wingman-rank', 'gold-nova-master',              null),
    ('cs2', 'wingman-rank',     'master-guardian-i',             'wingman-rank', 'master-guardian-1',             null),
    ('cs2', 'wingman-rank',     'master-guardian-ii',            'wingman-rank', 'master-guardian-2',             null),
    ('cs2', 'wingman-rank',     'master-guardian-elite',         'wingman-rank', 'master-guardian-elite',         null),
    ('cs2', 'wingman-rank',     'distinguished-master-guardian', 'wingman-rank', 'distinguished-master-guardian', null),
    ('cs2', 'wingman-rank',     'legendary-eagle',               'wingman-rank', 'legendary-eagle',               null),
    ('cs2', 'wingman-rank',     'legendary-eagle-master',        'wingman-rank', 'legendary-eagle-master',        null),
    ('cs2', 'wingman-rank',     'supreme-master-first-class',    'wingman-rank', 'supreme-master-first-class',    null),
    ('cs2', 'wingman-rank',     'the-global-elite',              'wingman-rank', 'the-global-elite',              null),
    ('cs2', 'faceit-rank', '1',  'faceit-level', 'level-1',  null),
    ('cs2', 'faceit-rank', '2',  'faceit-level', 'level-2',  null),
    ('cs2', 'faceit-rank', '3',  'faceit-level', 'level-3',  null),
    ('cs2', 'faceit-rank', '4',  'faceit-level', 'level-4',  null),
    ('cs2', 'faceit-rank', '5',  'faceit-level', 'level-5',  null),
    ('cs2', 'faceit-rank', '6',  'faceit-level', 'level-6',  null),
    ('cs2', 'faceit-rank', '7',  'faceit-level', 'level-7',  null),
    ('cs2', 'faceit-rank', '8',  'faceit-level', 'level-8',  null),
    ('cs2', 'faceit-rank', '9',  'faceit-level', 'level-9',  null),
    ('cs2', 'faceit-rank', '10', 'faceit-level', 'level-10', null),
    ('cs2', 'role', 'rifler',         'role',           'rifler',        null),
    ('cs2', 'role', 'awper',          'role',           'awper',         null),
    ('cs2', 'role', 'lurker',         'role',           'lurker',        null),
    ('cs2', 'role', 'entry-fragger',  'role',           'entry-fragger', null),
    ('cs2', 'role', 'supporter',      'role',           'support',       null),
    ('cs2', 'role', 'in-game-leader', 'in-game-leader', null,            null),
    ('cs2', 'interest', 'casual',              'looking-for', 'casual',             null),
    ('cs2', 'interest', 'deathmatch',          'looking-for', 'casual',             null),
    ('cs2', 'interest', 'wargames',            'looking-for', 'casual',             null),
    ('cs2', 'interest', 'competitive',         'looking-for', 'ranked',             null),
    ('cs2', 'interest', 'wingman',             'looking-for', 'ranked',             null),
    ('cs2', 'interest', 'faceit',              'looking-for', 'ranked',             null),
    ('cs2', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),

    -- Dota 2. A medal without its stars spans all five; Chinese servers are one.
    ('dota2', 'rank', 'herald',   'rank', 'herald-1',   'herald-5'),
    ('dota2', 'rank', 'guardian', 'rank', 'guardian-1', 'guardian-5'),
    ('dota2', 'rank', 'crusader', 'rank', 'crusader-1', 'crusader-5'),
    ('dota2', 'rank', 'archon',   'rank', 'archon-1',   'archon-5'),
    ('dota2', 'rank', 'legend',   'rank', 'legend-1',   'legend-5'),
    ('dota2', 'rank', 'ancient',  'rank', 'ancient-1',  'ancient-5'),
    ('dota2', 'rank', 'divine',   'rank', 'divine-1',   'divine-5'),
    ('dota2', 'rank', 'immortal', 'rank', 'immortal',   null),
    ('dota2', 'role', 'safe-lane',    'position', 'carry',        null),
    ('dota2', 'role', 'mid-lane',     'position', 'mid',          null),
    ('dota2', 'role', 'off-lane',     'position', 'offlane',      null),
    ('dota2', 'role', 'soft-support', 'position', 'soft-support', null),
    ('dota2', 'role', 'hard-support', 'position', 'hard-support', null),
    ('dota2', 'region', 'us-west',            'server', 'us-west',       null),
    ('dota2', 'region', 'us-east',            'server', 'us-east',       null),
    ('dota2', 'region', 'south-america',      'server', 'south-america', null),
    ('dota2', 'region', 'peru',               'server', 'peru',          null),
    ('dota2', 'region', 'europe-west',        'server', 'europe-west',   null),
    ('dota2', 'region', 'europe-east',        'server', 'europe-east',   null),
    ('dota2', 'region', 'russia',             'server', 'russia',        null),
    ('dota2', 'region', 'dubai',              'server', 'dubai',         null),
    ('dota2', 'region', 'south-africa',       'server', 'south-africa',  null),
    ('dota2', 'region', 'india',              'server', 'india',         null),
    ('dota2', 'region', 'se-asia',            'server', 'se-asia',       null),
    ('dota2', 'region', 'japan',              'server', 'japan',         null),
    ('dota2', 'region', 'australia',          'server', 'australia',     null),
    ('dota2', 'region', 'china-uc',           'server', 'china',         null),
    ('dota2', 'region', 'china-uc-2',         'server', 'china',         null),
    ('dota2', 'region', 'china-tc-wuhan',     'server', 'china',         null),
    ('dota2', 'region', 'china-tc-shanghai',  'server', 'china',         null),
    ('dota2', 'region', 'china-tc-guangdong', 'server', 'china',         null),
    ('dota2', 'region', 'china-tc-zhejiang',  'server', 'china',         null),
    ('dota2', 'battle-cup-tier', '3', 'battle-cup-tier', 'tier-3', null),
    ('dota2', 'battle-cup-tier', '4', 'battle-cup-tier', 'tier-4', null),
    ('dota2', 'battle-cup-tier', '5', 'battle-cup-tier', 'tier-5', null),
    ('dota2', 'battle-cup-tier', '6', 'battle-cup-tier', 'tier-6', null),
    ('dota2', 'battle-cup-tier', '7', 'battle-cup-tier', 'tier-7', null),
    ('dota2', 'battle-cup-tier', '8', 'battle-cup-tier', 'tier-8', null),
    ('dota2', 'interest', 'unranked',            'looking-for', 'casual',             null),
    ('dota2', 'interest', 'ranked',              'looking-for', 'ranked',             null),
    ('dota2', 'interest', 'battle-cup',          'looking-for', 'battle-cup',         null),
    ('dota2', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),

    -- Heroes of the Storm. Offlaners are the bruisers; a Flex player plays
    -- every role.
    ('hots', 'rank', 'bronze',       'rank', 'bronze-5',   'bronze-1'),
    ('hots', 'rank', 'silver',       'rank', 'silver-5',   'silver-1'),
    ('hots', 'rank', 'gold',         'rank', 'gold-5',     'gold-1'),
    ('hots', 'rank', 'platinum',     'rank', 'platinum-5', 'platinum-1'),
    ('hots', 'rank', 'diamond',      'rank', 'diamond-5',  'diamond-1'),
    ('hots', 'rank', 'master',       'rank', 'master',     null),
    ('hots', 'rank', 'grand-master', 'rank', 'grand-master', null),
    ('hots', 'role', 'tank',    'role', 'tank',    null),
    ('hots', 'role', 'offlane', 'role', 'bruiser', null),
    ('hots', 'role', 'healer',  'role', 'healer',  null),
    ('hots', 'role', 'dps',     'role', 'dps',     null),
    ('hots', 'role', 'flex',    'role', 'tank',    null),
    ('hots', 'role', 'flex',    'role', 'bruiser', null),
    ('hots', 'role', 'flex',    'role', 'healer',  null),
    ('hots', 'role', 'flex',    'role', 'dps',     null),
    ('hots', 'interest', 'quick-match',         'looking-for', 'casual',             null),
    ('hots', 'interest', 'unranked',            'looking-for', 'casual',             null),
    ('hots', 'interest', 'aram',                'looking-for', 'casual',             null),
    ('hots', 'interest', 'custom-games',        'looking-for', 'casual',             null),
    ('hots', 'interest', 'ranked',              'looking-for', 'ranked',             null),
    ('hots', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),

    -- League of Legends. Emerald came in between Platinum and Diamond, so the
    -- old tiers keep their names.
    ('lol', 'rank', 'iron',        'rank', 'iron-iv',     'iron-i'),
    ('lol', 'rank', 'bronze',      'rank', 'bronze-iv',   'bronze-i'),
    ('lol', 'rank', 'silver',      'rank', 'silver-iv',   'silver-i'),
    ('lol', 'rank', 'gold',        'rank', 'gold-iv',     'gold-i'),
    ('lol', 'rank', 'platinum',    'rank', 'platinum-iv', 'platinum-i'),
    ('lol', 'rank', 'diamond',     'rank', 'diamond-iv',  'diamond-i'),
    ('lol', 'rank', 'master',      'rank', 'master',      null),
    ('lol', 'rank', 'grandmaster', 'rank', 'grandmaster', null),
    ('lol', 'rank', 'Challenger',  'rank', 'challenger',  null),
    ('lol', 'role', 'top-lane', 'role', 'top',     null),
    ('lol', 'role', 'jungle',   'role', 'jungle',  null),
    ('lol', 'role', 'mid-lane', 'role', 'mid',     null),
    ('lol', 'role', 'ad-carry', 'role', 'adc',     null),
    ('lol', 'role', 'support',  'role', 'support', null),
    ('lol', 'clash-tier', '1', 'clash-tier', 'tier-i',   null),
    ('lol', 'clash-tier', '2', 'clash-tier', 'tier-ii',  null),
    ('lol', 'clash-tier', '3', 'clash-tier', 'tier-iii', null),
    ('lol', 'clash-tier', '4', 'clash-tier', 'tier-iv',  null),
    ('lol', 'interest', 'unranked-summoners-rift', 'looking-for', 'casual',             null),
    ('lol', 'interest', 'aram',                    'looking-for', 'casual',             null),
    ('lol', 'interest', 'nexus-blitz',             'looking-for', 'casual',             null),
    ('lol', 'interest', 'ranked-summoners-rift',   'looking-for', 'ranked',             null),
    ('lol', 'interest', 'clash',                   'looking-for', 'clash',              null),
    ('lol', 'interest', 'leagues-tournaments',     'looking-for', 'scrims-tournaments', null),

    -- Overwatch. Emerald came in between Platinum and Diamond, so the old tiers
    -- keep their names. The queue asks for tank, DPS or support, so the old
    -- sub-roles fold into those.
    ('overwatch', 'tank-rank', 'bronze',      'tank-rank', 'bronze-5',      'bronze-1'),
    ('overwatch', 'tank-rank', 'silver',      'tank-rank', 'silver-5',      'silver-1'),
    ('overwatch', 'tank-rank', 'gold',        'tank-rank', 'gold-5',        'gold-1'),
    ('overwatch', 'tank-rank', 'platinum',    'tank-rank', 'platinum-5',    'platinum-1'),
    ('overwatch', 'tank-rank', 'diamond',     'tank-rank', 'diamond-5',     'diamond-1'),
    ('overwatch', 'tank-rank', 'master',      'tank-rank', 'master-5',      'master-1'),
    ('overwatch', 'tank-rank', 'grandmaster', 'tank-rank', 'grandmaster-5', 'grandmaster-1'),
    ('overwatch', 'damage-rank', 'bronze',      'dps-rank', 'bronze-5',      'bronze-1'),
    ('overwatch', 'damage-rank', 'silver',      'dps-rank', 'silver-5',      'silver-1'),
    ('overwatch', 'damage-rank', 'gold',        'dps-rank', 'gold-5',        'gold-1'),
    ('overwatch', 'damage-rank', 'platinum',    'dps-rank', 'platinum-5',    'platinum-1'),
    ('overwatch', 'damage-rank', 'diamond',     'dps-rank', 'diamond-5',     'diamond-1'),
    ('overwatch', 'damage-rank', 'master',      'dps-rank', 'master-5',      'master-1'),
    ('overwatch', 'damage-rank', 'grandmaster', 'dps-rank', 'grandmaster-5', 'grandmaster-1'),
    ('overwatch', 'support-rank', 'bronze',      'support-rank', 'bronze-5',      'bronze-1'),
    ('overwatch', 'support-rank', 'silver',      'support-rank', 'silver-5',      'silver-1'),
    ('overwatch', 'support-rank', 'gold',        'support-rank', 'gold-5',        'gold-1'),
    ('overwatch', 'support-rank', 'platinum',    'support-rank', 'platinum-5',    'platinum-1'),
    ('overwatch', 'support-rank', 'diamond',     'support-rank', 'diamond-5',     'diamond-1'),
    ('overwatch', 'support-rank', 'master',      'support-rank', 'master-5',      'master-1'),
    ('overwatch', 'support-rank', 'grandmaster', 'support-rank', 'grandmaster-5', 'grandmaster-1'),
    ('overwatch', 'role', 'tank',           'role', 'tank',    null),
    ('overwatch', 'role', 'hitscan-dps',    'role', 'dps',     null),
    ('overwatch', 'role', 'projectile-dps', 'role', 'dps',     null),
    ('overwatch', 'role', 'main-support',   'role', 'support', null),
    ('overwatch', 'role', 'flex-support',   'role', 'support', null),
    ('overwatch', 'interest', 'unranked',            'looking-for', 'casual',             null),
    ('overwatch', 'interest', 'arcade',              'looking-for', 'casual',             null),
    ('overwatch', 'interest', 'custom-games',        'looking-for', 'casual',             null),
    ('overwatch', 'interest', 'competitive',         'looking-for', 'ranked',             null),
    ('overwatch', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),

    -- Rainbow Six Siege. Emerald came in between Platinum and Diamond. A Flex
    -- player plays every role; in-game leader is its own yes-or-no.
    ('r6s', 'rank', 'copper',    'rank', 'copper-v',   'copper-i'),
    ('r6s', 'rank', 'bronze',    'rank', 'bronze-v',   'bronze-i'),
    ('r6s', 'rank', 'silver',    'rank', 'silver-v',   'silver-i'),
    ('r6s', 'rank', 'gold',      'rank', 'gold-v',     'gold-i'),
    ('r6s', 'rank', 'platinum',  'rank', 'platinum-v', 'platinum-i'),
    ('r6s', 'rank', 'diamond',   'rank', 'diamond-v',  'diamond-i'),
    ('r6s', 'rank', 'champions', 'rank', 'champion-v', 'champion-i'),
    ('r6s', 'role', 'entry',          'role',           'entry',   null),
    ('r6s', 'role', 'support',        'role',           'support', null),
    ('r6s', 'role', 'anchor',         'role',           'anchor',  null),
    ('r6s', 'role', 'roamer',         'role',           'roamer',  null),
    ('r6s', 'role', 'flex',           'role',           'entry',   null),
    ('r6s', 'role', 'flex',           'role',           'support', null),
    ('r6s', 'role', 'flex',           'role',           'anchor',  null),
    ('r6s', 'role', 'flex',           'role',           'roamer',  null),
    ('r6s', 'role', 'in-game-leader', 'in-game-leader', null,      null),
    ('r6s', 'interest', 'quick-match',         'looking-for', 'casual',             null),
    ('r6s', 'interest', 'unranked',            'looking-for', 'casual',             null),
    ('r6s', 'interest', 'deathmatch',          'looking-for', 'casual',             null),
    ('r6s', 'interest', 'ranked',              'looking-for', 'ranked',             null),
    ('r6s', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),

    -- Team Fortress 2. Faceit no longer runs TF2, so its rank and league drop,
    -- and what it ran was organised play. Competitive sat beside leagues in the
    -- old options, so it was Valve's queue.
    ('tf2', 'rank', 'mercenary',       'rank', 'mercenary-i',       'mercenary-iii'),
    ('tf2', 'rank', 'contract-killer', 'rank', 'contract-killer-i', 'contract-killer-iii'),
    ('tf2', 'rank', 'executioner',     'rank', 'executioner-i',     'executioner-iii'),
    ('tf2', 'rank', 'expert-assasin',  'rank', 'expert-assassin-i', 'expert-assassin-iii'),
    ('tf2', 'rank', 'death-merchant',  'rank', 'death-merchant',    null),
    ('tf2', 'class', 'scout',    'class', 'scout',    null),
    ('tf2', 'class', 'soldier',  'class', 'soldier',  null),
    ('tf2', 'class', 'pyro',     'class', 'pyro',     null),
    ('tf2', 'class', 'demoman',  'class', 'demoman',  null),
    ('tf2', 'class', 'heavy',    'class', 'heavy',    null),
    ('tf2', 'class', 'engineer', 'class', 'engineer', null),
    ('tf2', 'class', 'medic',    'class', 'medic',    null),
    ('tf2', 'class', 'sniper',   'class', 'sniper',   null),
    ('tf2', 'class', 'spy',      'class', 'spy',      null),
    ('tf2', 'interest', 'casual',              'looking-for', 'casual',             null),
    ('tf2', 'interest', 'community-servers',   'looking-for', 'casual',             null),
    ('tf2', 'interest', 'competitive',         'looking-for', 'ranked',             null),
    ('tf2', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null),
    ('tf2', 'interest', 'faceit',              'looking-for', 'scrims-tournaments', null),
    ('tf2', 'interest', 'mann-vs-machine',     'looking-for', 'mann-vs-machine',    null),

    -- Valheim. The old server focus is the new playstyles.
    ('valheim', 'server-characters', 'new-characters',      'server-characters', 'new-characters',      null),
    ('valheim', 'server-characters', 'existing-characters', 'server-characters', 'existing-characters', null),
    ('valheim', 'server-type', 'vanilla', 'server-type', 'vanilla', null),
    ('valheim', 'server-type', 'modded',  'server-type', 'modded',  null),
    ('valheim', 'server-focus', 'pve',      'looking-for', 'pve',      null),
    ('valheim', 'server-focus', 'pvp',      'looking-for', 'pvp',      null),
    ('valheim', 'server-focus', 'building', 'looking-for', 'building', null),
    ('valheim', 'server-focus', 'roleplay', 'looking-for', 'roleplay', null),

    -- Valorant. Ascendant came in between Diamond and Immortal. The old roles
    -- were jobs on the team rather than agent roles: only the entry fragger is
    -- one agent role's job, and the in-game leader is its own yes-or-no.
    ('valorant', 'rank', 'iron',     'rank', 'iron-1',     'iron-3'),
    ('valorant', 'rank', 'bronze',   'rank', 'bronze-1',   'bronze-3'),
    ('valorant', 'rank', 'silver',   'rank', 'silver-1',   'silver-3'),
    ('valorant', 'rank', 'gold',     'rank', 'gold-1',     'gold-3'),
    ('valorant', 'rank', 'platinum', 'rank', 'platinum-1', 'platinum-3'),
    ('valorant', 'rank', 'diamond',  'rank', 'diamond-1',  'diamond-3'),
    ('valorant', 'rank', 'immortal', 'rank', 'immortal-1', 'immortal-3'),
    ('valorant', 'rank', 'radiant',  'rank', 'radiant',    null),
    ('valorant', 'role', 'entry-fragger',  'role',           'duelist', null),
    ('valorant', 'role', 'in-game-leader', 'in-game-leader', null,      null),
    ('valorant', 'interest', 'unrated',             'looking-for', 'casual',             null),
    ('valorant', 'interest', 'spike-rush',          'looking-for', 'casual',             null),
    ('valorant', 'interest', 'competitive',         'looking-for', 'ranked',             null),
    ('valorant', 'interest', 'leagues-tournaments', 'looking-for', 'scrims-tournaments', null);

-- Today's platforms are stores as much as devices; every PC store is PC. The
-- answer reaches a game only where it has a platform field with that option.
create table legacy.platform_map
    ( old_platform text primary key
    , new_platform text not null
    );

insert into legacy.platform_map (old_platform, new_platform) values
    ('steam', 'pc'),
    ('origin', 'pc'),
    ('riot', 'pc'),
    ('battle.net', 'pc'),
    ('ubisoft-connect', 'pc'),
    ('playstation', 'playstation'),
    ('xbox', 'xbox'),
    ('switch', 'switch');

-- Today's countries under the names the new list gives them. The rest of
-- today's leaves already match.
create table legacy.country_map
    ( old_name text primary key
    , new_name text not null
    );

insert into legacy.country_map (old_name, new_name) values
    ('Bahrein', 'Bahrain'),
    ('Czech Republic', 'Czechia'),
    ('Eswatini (Swaziland)', 'Eswatini'),
    ('Federated States of Micronesia', 'Micronesia'),
    ('Lichtenstein', 'Liechtenstein'),
    ('Luxemburg', 'Luxembourg'),
    ('North Asia (Russia)', 'Russia'),
    ('South Africa, Republic of', 'South Africa'),
    ('The Bahamas', 'Bahamas'),
    ('Timor Leste', 'Timor-Leste'),
    ('UAE', 'United Arab Emirates'),
    ('United States of America', 'United States');

-- The regions each of today's inner nodes covers, for a team's locations. A
-- country among them is read through its own region instead. A continent is
-- every region it takes in, which is what a team naming one meant; reading
-- the nodes through their countries instead drags Europe into North America by
-- way of Greenland.
create table legacy.location_region_map
    ( old_name text not null
    , region_name text not null
    , primary key (old_name, region_name)
    );

insert into legacy.location_region_map (old_name, region_name) values
    ('Europe', 'Europe'),
    ('West Europe', 'Europe'),
    ('East Europe', 'Europe'),
    ('North Europe', 'Europe'),
    ('North America', 'North America'),
    ('North America', 'Central America'),
    ('Central America', 'Central America'),
    ('Caribbean', 'Central America'),
    ('South America', 'South America'),
    ('North South America', 'South America'),
    ('South South America', 'South America'),
    ('West South America', 'South America'),
    ('Africa', 'North Africa'),
    ('Africa', 'Sub-Saharan Africa'),
    ('North Africa', 'North Africa'),
    ('West Africa', 'Sub-Saharan Africa'),
    ('East Africa', 'Sub-Saharan Africa'),
    ('Central Africa', 'Sub-Saharan Africa'),
    ('South Africa', 'Sub-Saharan Africa'),
    ('Asia', 'Middle East'),
    ('Asia', 'Central Asia'),
    ('Asia', 'South Asia'),
    ('Asia', 'East Asia'),
    ('Asia', 'Southeast Asia'),
    ('West Asia', 'Middle East'),
    ('Central Asia', 'Central Asia'),
    ('South Asia', 'South Asia'),
    ('East Asia', 'East Asia'),
    ('Southeast Asia', 'Southeast Asia'),
    ('Oceania', 'Oceania'),
    ('Melanesia', 'Oceania'),
    ('Micronesia', 'Oceania'),
    ('Polynesia', 'Oceania');
