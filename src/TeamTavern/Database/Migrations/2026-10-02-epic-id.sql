-- The Epic ID, the account Rocket League players add each other by on every
-- platform, as a contact a game can offer.

begin;

alter table player add column epic_id text;

alter table game_contact drop constraint game_contact_kind_check;
alter table game_contact add constraint game_contact_kind_check check (kind in
    ('discord', 'steam', 'riot', 'battle_tag', 'ea', 'epic'
    , 'ubisoft', 'marvel_rivals', 'psn', 'gamer_tag', 'friend_code'));

commit;
