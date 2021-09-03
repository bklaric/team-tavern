begin;
-- Move about to profiles.
alter table player_profile add about text[];
alter table team_profile add about text[];

update player_profile
set about = player.about
from player
where player.id = player_profile.player_id;

update team_profile
set about = team.about
from team
where team.id = team_profile.team_id;

alter table player drop about;
alter table team drop about;

alter table player_profile alter about set not null;
alter table team_profile alter about set not null;

-- Add contacts.
alter table player add steam_id text;
alter table player add riot_id text;
alter table player add battle_tag text;
alter table player add psn_id text;
alter table player add gamer_tag text;
alter table player add friend_code text;

alter table team add steam_id text;
alter table team add riot_id text;
alter table team add battle_tag text;
alter table team add psn_id text;
alter table team add gamer_tag text;
alter table team add friend_code text;

update player
set
    steam_id = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'steam' limit 1),
    riot_id = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'riot' limit 1),
    battle_tag = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'battle.net' limit 1),
    psn_id = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'playstation' limit 1),
    gamer_tag = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'xbox' limit 1),
    friend_code = (select platform_id from player_profile where player_profile.player_id = player.id and platform = 'switch' limit 1);

alter table player_profile drop platform_id;
commit;
