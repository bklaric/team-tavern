begin;

create table tracker (
    id serial not null primary key,
    game_id int not null references game(id),
    platform text not null,
    title text not null,
    template text not null
);

insert into tracker (game_id, platform, title, template) values

-- Insert Dota 2 trackers.
((select id from game where handle = 'dota2'), 'steam', 'opendota.com', 'https://www.opendota.com/players/'),
((select id from game where handle = 'dota2'), 'steam', 'dotabuff.com', 'https://www.dotabuff.com/players/'),

-- Insert League of Legends trackers.
-- LoL trackers don't work because all of them ask for summoner name and region, RiotID doesn't work.

-- Insert Heroes of the Storm trackers.
-- Doesn't really work.
-- ((select id from game where handle = 'hots'), 'battle.net', 'hotslogs.com', 'https://www.hotslogs.com/Player/Profile?PlayerID='),

-- Insert CS:GO trackers.
((select id from game where handle = 'csgo'), 'steam', 'tracker.gg', 'https://tracker.gg/csgo/profile/steam/'),
((select id from game where handle = 'csgo'), 'steam', 'csgostats.gg', 'https://csgostats.gg/player/'),
-- Has some CSRF protection or something, so we can't link to it.
-- ((select id from game where handle = 'csgo'), 'steam', 'csgorankings.com', 'https://csgorankings.com/profile/'),

-- Insert Valorant trackers.
((select id from game where handle = 'valorant'), 'riot', 'tracker.gg', 'https://tracker.gg/valorant/profile/riot/'),
((select id from game where handle = 'valorant'), 'riot', 'blitz.gg', 'https://blitz.gg/valorant/profile/'),

-- Insert Rainbow Six: Siege trackers.
-- Different link for each platform is needed.
-- ((select id from game where handle = 'r6s'), 'ubisoft-connect', 'r6.tracker.network', 'https://r6.tracker.network/profile/'),

-- Insert Overwatch trackers.
((select id from game where handle = 'overwatch'), 'battle.net', 'tracker.gg', 'https://tracker.gg/overwatch/profile/battlenet/'),
((select id from game where handle = 'overwatch'), 'battle.net', 'overbuff.com', 'https://www.overbuff.com/players/'),

-- Import Team Fortress 2 trackers.
((select id from game where handle = 'tf2'), 'steam', 'tr2center.com', 'https://tf2center.com/profile/'),

-- Insert Apex Legends trackers.
((select id from game where handle = 'apex'), 'origin', 'tracker.gg', 'https://tracker.gg/apex/profile/origin/'),
((select id from game where handle = 'apex'), 'playstation', 'tracker.gg', 'https://tracker.gg/apex/profile/psn/'),
((select id from game where handle = 'apex'), 'steam', 'tracker.gg', 'https://tracker.gg/apex/profile/steam/'),

-- Insert Splitgate trackers.
((select id from game where handle = 'splitgate'), 'xbox', 'tracker.gg', 'https://tracker.gg/splitgate/profile/xbl/'),
((select id from game where handle = 'splitgate'), 'playstation', 'tracker.gg', 'https://tracker.gg/splitgate/profile/psn/'),
((select id from game where handle = 'splitgate'), 'steam', 'tracker.gg', 'https://tracker.gg/splitgate/profile/steam/');

-- Insert Valheim trackers.
-- There are none.

commit;
