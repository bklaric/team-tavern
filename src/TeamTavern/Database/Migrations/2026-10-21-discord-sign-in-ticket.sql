-- Discord signs in with a code it swaps once, as Google does, so a Discord
-- account new to the site registers with a sign-in ticket too, which keeps the
-- account's tag for the player's Discord contact.

begin;

alter table sign_in_ticket drop constraint sign_in_ticket_provider_check;
alter table sign_in_ticket add constraint sign_in_ticket_provider_check
    check (provider in ('discord', 'steam', 'google'));

alter table sign_in_ticket add column discord_tag varchar(37);

commit;
