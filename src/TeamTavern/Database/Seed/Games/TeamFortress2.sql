insert into game (title, short_title, handle, description, platforms)
values (
    'Team Fortress 2',
    'TF2',
    'tf2',
    array['Find TF2 teammates for casual, competitive, MvM, community servers and more.']::varchar[],
    array['steam']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'tf2'), 'single', 'Rank', 'rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'tf2'), 'single', 'Faceit rank', 'faceit-rank', 'fas fa-medal', 2),
    ((select id from game where game.handle = 'tf2'), 'single', 'Faceit league', 'faceit-league', 'fas fa-medal', 3),
    ((select id from game where game.handle = 'tf2'), 'multi', 'Class', 'class', 'fas fa-bullseye', 4),
    ((select id from game where game.handle = 'tf2'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 5);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Mercenary', 'mercenary', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Contract Killer', 'contract-killer', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Executioner', 'executioner', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Expert Assasin', 'expert-assasin', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Death Merchant', 'death-merchant', 5),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '1', '1', 1),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '2', '2', 2),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '3', '3', 3),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '4', '4', 4),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '5', '5', 5),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '6', '6', 6),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '7', '7', 7),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '8', '8', 8),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '9', '9', 9),
    ((select id from field where field.key = 'faceit-rank' and field.game_id = ((select id from game where game.handle = 'tf2'))), '10', '10', 10),
    ((select id from field where field.key = 'faceit-league' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Unremarkable', 'unremarkable', 1),
    ((select id from field where field.key = 'faceit-league' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Notably Dangerous', 'notably-dangerous', 2),
    ((select id from field where field.key = 'faceit-league' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Face Melting', 'face-melting', 3),
    ((select id from field where field.key = 'faceit-league' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Server Clearing', 'server-clearing', 4),
    ((select id from field where field.key = 'faceit-league' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Hale''s Own', 'hales-own', 5),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Scout', 'scout', 1),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Soldier', 'soldier', 2),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Pyro', 'pyro', 3),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Demoman', 'demoman', 4),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Heavy', 'heavy', 5),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Engineer', 'engineer', 6),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Medic', 'medic', 7),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Sniper', 'sniper', 8),
    ((select id from field where field.key = 'class' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Spy', 'spy', 9),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Casual', 'casual', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Competitive', 'competitive', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Mann vs. Machine', 'mann-vs-machine', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Community servers', 'community-servers', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Faceit', 'faceit', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'tf2'))), 'Leagues/tournaments', 'leagues-tournaments', 6);

insert into tracker (game_id, platform, title, template)
values
    ((select id from game where game.handle = 'tf2'), 'steam', 'tr2center.com', 'https://tf2center.com/profile/');
