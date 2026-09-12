insert into game (title, short_title, handle, description, platforms)
values (
    'Overwatch',
    'Overwatch',
    'overwatch',
    array['Find Overwatch teammates for quick play, arcade, competitive playe and more.']::varchar[],
    array['battle.net', 'playstation', 'xbox', 'switch']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'overwatch'), 'single', 'Tank rank', 'tank-rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'overwatch'), 'single', 'Damage rank', 'damage-rank', 'fas fa-medal', 2),
    ((select id from game where game.handle = 'overwatch'), 'single', 'Support rank', 'support-rank', 'fas fa-medal', 3),
    ((select id from game where game.handle = 'overwatch'), 'multi', 'Role', 'role', 'fas fa-bullseye', 4),
    ((select id from game where game.handle = 'overwatch'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 5);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'tank-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Grandmaster', 'grandmaster', 7),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'damage-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Grandmaster', 'grandmaster', 7),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'support-rank' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Grandmaster', 'grandmaster', 7),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Tank', 'tank', 1),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Hitscan DPS', 'hitscan-dps', 2),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Projectile DPS', 'projectile-dps', 3),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Main support', 'main-support', 4),
    ((select id from field where field.key = 'role' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Flex support', 'flex-support', 5),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Unranked', 'unranked', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Competitive', 'competitive', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Arcade', 'arcade', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Custom games', 'custom-games', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'overwatch'))), 'Leagues/tournaments', 'leagues-tournaments', 5);

insert into tracker (game_id, platform, title, template)
values
    ((select id from game where game.handle = 'overwatch'), 'battle.net', 'tracker.gg', 'https://tracker.gg/overwatch/profile/battlenet/'),
    ((select id from game where game.handle = 'overwatch'), 'battle.net', 'overbuff.com', 'https://www.overbuff.com/players/');
