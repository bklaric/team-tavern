begin;

update tracker
set
    title = 'csstats.gg',
    template = 'https://csstats.gg/player/'
where
    title = 'csgostats.gg';

commit;
