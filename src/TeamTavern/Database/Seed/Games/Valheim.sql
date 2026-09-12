insert into game (title, short_title, handle, description, platforms)
values (
    'Valheim',
    'Valheim',
    'valheim',
    array['Join Valheim servers looking for players or find players for your own server.']::varchar[],
    array['steam']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'valheim'), 'multi', 'Server characters', 'server-characters', 'fas fa-user-plus', 1),
    ((select id from game where game.handle = 'valheim'), 'multi', 'Server type', 'server-type', 'fas fa-server', 2),
    ((select id from game where game.handle = 'valheim'), 'multi', 'Server focus', 'server-focus', 'fas fa-handshake', 3);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'server-characters' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'New characters', 'new-characters', 1),
    ((select id from field where field.key = 'server-characters' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'Existing characters', 'existing-characters', 2),
    ((select id from field where field.key = 'server-type' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'Vanilla', 'vanilla', 1),
    ((select id from field where field.key = 'server-type' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'Modded', 'modded', 2),
    ((select id from field where field.key = 'server-focus' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'PvE', 'pve', 1),
    ((select id from field where field.key = 'server-focus' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'PvP', 'pvp', 2),
    ((select id from field where field.key = 'server-focus' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'Roleplay', 'roleplay', 3),
    ((select id from field where field.key = 'server-focus' and field.game_id = ((select id from game where game.handle = 'valheim'))), 'Building', 'building', 4);
