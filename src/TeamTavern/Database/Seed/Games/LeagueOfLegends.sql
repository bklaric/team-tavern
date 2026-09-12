insert into game (title, short_title, handle, description, platforms)
values (
    'League of Legends',
    'LoL',
    'lol',
    array['Find LoL teammates for Summoner''s Rift, Aram, Nexus Blitz and more.']::varchar[],
    array['riot']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'lol'), 'single', 'Rank', 'rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'lol'), 'multi', 'Role', 'role', 'fas fa-bullseye', 2),
    ((select id from game where game.handle = 'lol'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 3),
    ((select id from game where game.handle = 'lol'), 'multi', 'Clash tier', 'clash-tier', 'fas fa-trophy', 4);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Iron', 'iron', 1),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Bronze', 'bronze', 2),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Silver', 'silver', 3),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Gold', 'gold', 4),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Platinum', 'platinum', 5),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Diamond', 'diamond', 6),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Master', 'master', 7),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Grandmaster', 'grandmaster', 8),
    ((select id from field where field.key = 'rank' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Challenger', 'Challenger', 9),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'lol'))), 'AD carry', 'ad-carry', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Mid lane', 'mid-lane', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Top lane', 'top-lane', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Jungle', 'jungle', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Support', 'support', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Unranked Summoner''s Rift', 'unranked-summoners-rift', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Ranked Summoner''s Rift', 'ranked-summoners-rift', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Aram', 'aram', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Nexus Blitz', 'nexus-blitz', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Clash', 'clash', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'lol'))), 'Leagues/tournaments', 'leagues-tournaments', 6),
    ((select id from field where field.key = 'clash-tier' and field.game_id = ((select id from game where game.handle = 'lol'))), '1', '1', 1),
    ((select id from field where field.key = 'clash-tier' and field.game_id = ((select id from game where game.handle = 'lol'))), '2', '2', 2),
    ((select id from field where field.key = 'clash-tier' and field.game_id = ((select id from game where game.handle = 'lol'))), '3', '3', 3),
    ((select id from field where field.key = 'clash-tier' and field.game_id = ((select id from game where game.handle = 'lol'))), '4', '4', 4);
