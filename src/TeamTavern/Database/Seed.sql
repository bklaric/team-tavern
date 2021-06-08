insert into competition (game_id, handle, name, description, website, discord_server, region) values
    ((select id from game where handle = 'dota2'), 'ad2l', 'Amateur Dota 2 League', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'atdl', 'Amateur Teams Dota League', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'friday', 'Friday''s League', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'ld2l', 'Learn Dota 2 League', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'md2l', 'Midwest Dota 2 League', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'pgc', 'PGC', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'rd2l-eu', 'Reddit Dota 2 League EU', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'rd2l-na', 'Reddit Dota 2 League NA', array['Description.']::text[], '', '', 'Europe'),
    ((select id from game where handle = 'dota2'), 'wd2l', 'West Coast Dota 2 League', array['Description.']::text[], '', '', 'Europe');
