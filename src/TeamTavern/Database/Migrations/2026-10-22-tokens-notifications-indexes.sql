-- The worker emails each notification once by a flag on it, rather than by the
-- time it was created. Reset and confirmation links are kept as hashes, as
-- session tokens are, and are deleted once used. The nickname loses its
-- case-sensitive unique constraint, which player_lower_nickname_key covers, and
-- the report table gets the indexes its cascades read.

begin;

-- The rows already there count as emailed, though those since the worker's
-- last run before the deploy go without.
alter table notification add column email_pending boolean not null default false;
alter table notification alter column email_pending set default true;
create index notification_email_pending_idx on notification (created) where email_pending;

delete from password_reset where consumed or created < now() - interval '1 hour';
alter table password_reset add column nonce_hash character(64);
update password_reset set nonce_hash = encode(sha256(convert_to(nonce, 'UTF8')), 'hex');
alter table password_reset alter column nonce_hash set not null;
alter table password_reset drop column nonce;
alter table password_reset drop column consumed;
create index password_reset_player_id_idx on password_reset (player_id);
create index password_reset_nonce_hash_idx on password_reset (nonce_hash);

delete from email_confirmation where consumed;
alter table email_confirmation add column nonce_hash character(64);
update email_confirmation set nonce_hash = encode(sha256(convert_to(nonce, 'UTF8')), 'hex');
alter table email_confirmation alter column nonce_hash set not null;
alter table email_confirmation drop column nonce;
alter table email_confirmation drop column consumed;
create index email_confirmation_nonce_hash_idx on email_confirmation (nonce_hash);

alter table player drop constraint player_nickname_key;

create index report_reporter_id_idx on report (reporter_id);
create index report_post_id_idx on report (post_id);

commit;
