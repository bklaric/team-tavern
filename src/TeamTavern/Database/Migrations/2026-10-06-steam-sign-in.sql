-- A Steam account as a third way to sign in, beside a password and a Discord
-- account, and the ticket a player new to the site registers with once Steam
-- has vouched for their account.

begin;

alter table player add column steam_sign_in_id text unique;

alter table player drop constraint player_identity_check;
alter table player add constraint player_identity_check
    check (num_nonnulls(password_hash, discord_id, steam_sign_in_id) = 1);

create table steam_ticket
    ( token_hash character(64) not null primary key
    , steam_id text not null
    , created timestamptz not null default current_timestamp
    );

create table steam_nonce
    ( nonce text not null primary key
    , accepted timestamptz not null default current_timestamp
    );

commit;
