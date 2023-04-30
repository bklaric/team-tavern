insert into game (title, short_title, handle, description, platforms)
values (
    'Rainbow Six: Siege',
    'R6S',
    'r6s',
    array['Find Rainbow Six: Siege teammates for lots of stuff, I need to check first.']::varchar[],
    array['steam', 'ubisoft-connect', 'playstation', 'xbox']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'r6s'), 'single', 'Rank', 'rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'r6s'), 'multi', 'Role', 'role', 'fas fa-bullseye', 2),
    ((select id from game where game.handle = 'r6s'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 3);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Copper', 'copper', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Bronze', 'bronze', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Silver', 'silver', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Gold', 'gold', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Platinum', 'platinum', 5),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Diamond', 'diamond', 6),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Champions', 'champions', 7),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'In-game leader', 'in-game-leader', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Entry', 'entry', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Support', 'support', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Anchor', 'anchor', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Roamer', 'roamer', 5),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Flex', 'flex', 6),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Quick match', 'quick-match', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Deathmatch', 'deathmatch', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Unranked', 'unranked', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Ranked', 'ranked', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'r6s'))), 'Leagues/tournaments', 'leagues-tournaments', 5);
