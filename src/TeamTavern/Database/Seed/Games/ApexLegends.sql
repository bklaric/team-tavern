alter table player add ea_id text;
alter table team add ea_id text;

insert into game (title, short_title, handle, description, platforms)
values (
    'Apex Legends',
    'Apex Legends',
    'apex',
    array['Find Apex Legends teammates for battle royale, arenas and more.']::varchar[],
    array['steam', 'origin', 'playstation', 'xbox', 'switch']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'apex'), 2, 'Battle royale rank', 'battle-royale-rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'apex'), 2, 'Arenas rank', 'arenas-rank', 'fas fa-medal', 2),
    ((select id from game where game.handle = 'apex'), 3, 'Interest', 'interest', 'fas fa-crosshairs', 3);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'battle-royale-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Apex predator', 'apex-predator', 7),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'arenas-rank' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Apex predator', 'apex-predator', 7),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Unranked battle royale', 'unranked-battle-royale', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Ranked battle royale', 'ranked-battle-royale', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Unranked arenas', 'unranked-arenas', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Ranked arenas', 'ranked-arenas', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'apex'))), 'Leagues/tournaments', 'leagues-tournaments', 5);
