update player_profile set ambitions =
    case
        when ambitions = about then ambitions
        else about || ambitions
    end
from player
where player.id = player_profile.player_id;

alter table player drop about;

alter table player_profile rename ambitions to about;


update team_profile set ambitions =
    case
        when ambitions = about then ambitions
        else about || ambitions
    end
from team
where team.id = team_profile.team_id;

alter table team drop about;

alter table team_profile rename ambitions to about;
