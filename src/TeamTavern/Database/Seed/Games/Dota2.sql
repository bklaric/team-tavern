insert into game (title, short_title, handle, description, platforms)
values (
    'Dota 2',
    'Dota 2',
    'dota2',
    array['Find Dota 2 players for ranked matches, unranked matches, battle cups and more.']::varchar[],
    array['steam']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'dota2'), 'single', 'Rank', 'rank', 'fas fa-medal', 4),
    ((select id from game where game.handle = 'dota2'), 'multi', 'Region', 'region', 'fas fa-globe-europe', 5),
    ((select id from game where game.handle = 'dota2'), 'multi', 'Role', 'role', 'fas fa-bullseye', 6),
    ((select id from game where game.handle = 'dota2'), 'multi', 'Battle cup tier', 'battle-cup-tier', 'fas fa-trophy', 7),
    ((select id from game where game.handle = 'dota2'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 8);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Herald', 'herald', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Guardian', 'guardian', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Crusader', 'crusader', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Archon', 'archon', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Legend', 'legend', 5),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Ancient', 'ancient', 6),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Divine', 'divine', 7),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Immortal', 'immortal', 8),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Europe East', 'europe-east', 1),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Europe West', 'europe-west', 2),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Russia', 'russia', 3),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'US East', 'us-east', 4),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'US West', 'us-west', 5),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'SE Asia', 'se-asia', 6),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Japan', 'japan', 7),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Australia', 'australia', 8),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'India', 'india', 9),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Peru', 'peru', 10),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'South America', 'south-america', 11),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Dubai', 'dubai', 12),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'South Africa', 'south-africa', 13),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China UC', 'china-uc', 14),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China UC 2', 'china-uc-2', 15),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China TC Wuhan', 'china-tc-wuhan', 16),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China TC Shanghai', 'china-tc-shanghai', 17),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China TC Guangdong', 'china-tc-guangdong', 18),
    ((select id from field where field.key = 'region' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'China TC Zhejiang', 'china-tc-zhejiang', 19),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Safe lane', 'safe-lane', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Mid lane', 'mid-lane', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Off lane', 'off-lane', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Soft support', 'soft-support', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Hard support', 'hard-support', 5),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '3', '3', 1),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '4', '4', 2),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '5', '5', 3),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '6', '6', 4),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '7', '7', 5),
    ((select id from field where field.key = 'battle-cup-tier' and field.game_id = ((select id from game where game.handle = 'dota2'))), '8', '8', 6),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Unranked', 'unranked', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Ranked', 'ranked', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Battle cup', 'battle-cup', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'dota2'))), 'Leagues/tournaments', 'leagues-tournaments', 4);

insert into tracker (game_id, platform, title, template)
values
    ((select id from game where game.handle = 'dota2'), 'steam', 'opendota.com', 'https://www.opendota.com/players/'),
    ((select id from game where game.handle = 'dota2'), 'steam', 'dotabuff.com', 'https://www.dotabuff.com/players/');
