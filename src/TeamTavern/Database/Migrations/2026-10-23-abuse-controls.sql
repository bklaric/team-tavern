-- A player reports a player about a post once. Repeats already stored keep the
-- first, and the unique constraint's index, which leads with reporter_id, takes
-- the place of report_reporter_id_idx.

begin;

delete from report
using report earlier
where report.reporter_id = earlier.reporter_id
    and report.reported_id = earlier.reported_id
    and report.post_id = earlier.post_id
    and report.id > earlier.id;

alter table report add constraint report_reporter_id_reported_id_post_id_key
    unique (reporter_id, reported_id, post_id);

drop index report_reporter_id_idx;

commit;
