begin;
alter table player add column steam_url text, add column riot_id text;
update player set
  steam_url = (select external_id from player_profile join game on game.id = player_profile.game_id where player.id = player_profile.player_id and external_id_ilk = 1 limit 1 ),
  riot_id = (select external_id from player_profile join game on game.id = player_profile.game_id where player.id = player_profile.player_id and external_id_ilk = 2 limit 1 )
commit;
