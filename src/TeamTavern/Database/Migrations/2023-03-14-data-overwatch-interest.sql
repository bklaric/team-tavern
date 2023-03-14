begin;

update field_option
set key = 'unranked', label = 'Unranked'
where id = 182;

update field_option
set key = 'competitive', label = 'Competitive'
where id = 184;

insert into field_option (field_id, key, label, ordinal)
values (31, 'custom-games', 'Custom games', 4);

update field_option
set ordinal =
    case
    when key = 'unranked' then 1
    when key = 'competitive' then 2
    when key = 'arcade' then 3
    when key = 'custom-games' then 4
    when key = 'leagues-tournaments' then 5
    end
where field_id = 31;

commit;
