insert into game (title, short_title, handle, description, platforms)
values (
    'Heroes of the Storm',
    'HotS',
    'hots',
    array['Find Heroes of the Storm teammates for unranked, ranked, ARAM and more.']::varchar[],
    array['battle.net']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'hots'), 'single', 'Rank', 'rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'hots'), 'multi', 'Role', 'role', 'fas fa-bullseye', 2),
    ((select id from game where game.handle = 'hots'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 3);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Grand Master', 'grand-master', 7),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Tank', 'tank', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Offlane', 'offlane', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'hots'))), 'DPS', 'dps', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Healer', 'healer', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Flex', 'flex', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Quick match', 'quick-match', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Unranked', 'unranked', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Ranked', 'ranked', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'ARAM', 'aram', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Custom games', 'custom-games', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'hots'))), 'Leagues/tournaments', 'leagues-tournaments', 6);
