begin;
alter table player add discord_id text unique;
alter table player alter password_hash drop not null;
commit;
