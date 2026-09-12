insert into game (title, short_title, handle, description, platforms)
values (
    'Valorant',
    'Valorant',
    'valorant',
    array['Find Valorant teammates for unrated, competitive, spike rush matches and more.']::varchar[],
    array['riot']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'valorant'), 'single', 'Rank', 'rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'valorant'), 'multi', 'Role', 'role', 'fas fa-bullseye', 2),
    ((select id from game where game.handle = 'valorant'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 3);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Iron', 'iron', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Bronze', 'bronze', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Silver', 'silver', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Gold', 'gold', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Platinum', 'platinum', 5),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Diamond', 'diamond', 6),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Immortal', 'immortal', 7),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Radiant', 'radiant', 8),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'In-game leader', 'in-game-leader', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Entry fragger', 'entry-fragger', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Sniper', 'sniper', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Lurker', 'lurker', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Supporter', 'supporter', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Unrated', 'unrated', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Competitive', 'competitive', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Spike Rush', 'spike-rush', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'valorant'))), 'Leagues/tournaments', 'leagues-tournaments', 4);

insert into tracker (game_id, platform, title, template)
values
    ((select id from game where game.handle = 'valorant'), 'riot', 'tracker.gg', 'https://tracker.gg/valorant/profile/riot/'),
    ((select id from game where game.handle = 'valorant'), 'riot', 'blitz.gg', 'https://blitz.gg/valorant/profile/');
