-- A Google account as a fourth way to sign in, beside a password, a Discord
-- account and a Steam account. The ticket a player new to the site registers
-- with moves to one table for Steam and Google; a ticket lasts an hour, so
-- those in steam_ticket go with it.

begin;

alter table player add column google_id text unique;

alter table player drop constraint player_identity_check;
alter table player add constraint player_identity_check
    check (num_nonnulls(password_hash, discord_id, steam_sign_in_id, google_id) = 1);

drop table steam_ticket;

create table sign_in_ticket
    ( token_hash character(64) not null primary key
    , provider text not null check (provider in ('steam', 'google'))
    , provider_id text not null
    , email varchar(254)
    , email_confirmed boolean not null
    , created timestamptz not null default current_timestamp
    );

commit;
