-- Clears every birthday that makes its player younger than 16, the terms'
-- minimum age, which the API now holds new birthdays to. Their posts then show
-- no age. It changes rows only, so TablesCurrent.sql stays as it is.
--
-- How many it clears:
--
--   select count(*) from player where birthday > current_date - interval '16 years';

begin;

update player
set birthday = null
where birthday > current_date - interval '16 years';

commit;
