-- Brings a database at TablesBase.sql up to TablesCurrent.sql. It changes no
-- rows, so a row that breaks the new rules fails it. Both counts must be 0
-- before it runs:
--
--   select count(*) from player where num_nonnulls(password_hash, discord_id) <> 1;
--
--   select count(*) from (
--       select lower(email) from player
--       where password_hash is not null and email is not null
--       group by lower(email) having count(*) > 1
--   ) duplicates;

begin;

-- The sign-in identity: a password or a Discord account, exactly one.
alter table player add constraint player_identity_check
    check (num_nonnulls(password_hash, discord_id) = 1);

-- The email is the contact email. A password player may sign in with it, so it
-- is unique among those only, and a Discord player may share a password player's.
alter table player drop constraint player_email_key;
drop index player_lower_email_key;
create unique index player_lower_email_key on player (lower(email))
    where password_hash is not null;

commit;
