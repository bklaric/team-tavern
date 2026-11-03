-- The Delta Force ID, the number on a player's profile that Delta Force players
-- add each other by on every platform, as a contact a game can offer.

begin;

alter table player add column delta_force_id text;

alter table game_contact drop constraint game_contact_kind_check;
alter table game_contact add constraint game_contact_kind_check check (kind in
    ('discord', 'steam', 'riot', 'battle_tag', 'ea', 'epic', 'embark', 'pubg', 'delta_force'
    , 'ubisoft', 'marvel_rivals', 'psn', 'gamer_tag', 'friend_code'));

commit;
