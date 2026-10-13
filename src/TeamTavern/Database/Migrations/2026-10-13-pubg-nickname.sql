-- The PUBG nickname, the name PUBG players on PC add each other by on Steam,
-- Epic and Kakao alike, as a contact a game can offer.

begin;

alter table player add column pubg_nickname text;

alter table game_contact drop constraint game_contact_kind_check;
alter table game_contact add constraint game_contact_kind_check check (kind in
    ('discord', 'steam', 'riot', 'battle_tag', 'ea', 'epic', 'embark', 'pubg'
    , 'ubisoft', 'marvel_rivals', 'psn', 'gamer_tag', 'friend_code'));

commit;
