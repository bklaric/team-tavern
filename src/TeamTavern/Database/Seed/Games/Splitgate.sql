insert into game (title, short_title, handle, description, platforms)
values (
    'Splitgate',
    'Splitgate',
    'splitgate',
    array['Find Splitgate teammates for casual, ranked 4v4, ranked takedown, ranked 2v2 and more.']::varchar[],
    array['steam', 'playstation', 'xbox']::varchar[]
);

insert into field (game_id, ilk, label, key, icon, ordinal)
values
    ((select id from game where game.handle = 'splitgate'), 'single', '4v4 rank', '4v4-rank', 'fas fa-medal', 1),
    ((select id from game where game.handle = 'splitgate'), 'single', 'Takedown rank', 'takedown-rank', 'fas fa-medal', 2),
    ((select id from game where game.handle = 'splitgate'), 'single', '2v2 rank', '2v2-rank', 'fas fa-medal', 3),
    ((select id from game where game.handle = 'splitgate'), 'multi', 'Interest', 'interest', 'fas fa-crosshairs', 4);

insert into field_option (field_id, label, key, ordinal)
values
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Master', 'master', 6),
    ((select id from field where field.key = '4v4-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Champion', 'champion', 7),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Master', 'master', 6),
    ((select id from field where field.key = 'takedown-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Champion', 'champion', 7),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Bronze', 'bronze', 1),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Silver', 'silver', 2),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Gold', 'gold', 3),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Platinum', 'platinum', 4),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Diamond', 'diamond', 5),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Master', 'master', 6),
    ((select id from field where field.key = '2v2-rank' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Champion', 'champion', 7),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Casual', 'casual', 1),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Ranked 4v4', 'ranked-4v4', 2),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Ranked takedown', 'ranked-takedown', 3),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Ranked 2v2', 'ranked-2v2', 4),
    ((select id from field where field.key = 'interest' and field.game_id = ((select id from game where game.handle = 'splitgate'))), 'Leagues/tournaments', 'leagues-tournaments', 5);
