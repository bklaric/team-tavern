-- Team Fortress 2
--
-- Two ladders, and the one players state leads. Division is the league season a
-- post plays in, ETF2L's in Europe and RGL's in North America on one ordered
-- ladder labelled with both names, and it is what a TF2 post says about its
-- level. Valve's own competitive rank ships with the game and so is here too,
-- behind Details: its queue sits empty, so almost no post will carry one. A
-- rank range spans either ladder and rank closeness counts in steps of one
-- (brief 7.2). Which league a post plays in follows from its regions, so it is
-- not a field of its own.
--
-- Classes are the nine, in the game's own offense, defense and support order.
-- They are slotted: Highlander fields one of each, so two players fit by
-- covering two different ones, and a roster short a Medic is short exactly that.
--
-- Format is what a competitive post plays, and a pug community answers it too.
--
-- Server type is what a community server runs, which is where most of TF2 is
-- played, so all three post types answer it and a community post is often
-- nothing but this field and its words.
--
-- Team Fortress 2 runs on PC alone, so there is no platform field.

insert into game (title, short_title, handle, description)
values
    ( 'Team Fortress 2'
    , 'TF2'
    , 'tf2'
    , array['Find Team Fortress 2 players, groups and communities: a class main for your roster, a team to play 6s or Highlander with, or a server to call home.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam')) as contact (kind)
where game.handle = 'tf2';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('division',    'Division',    'single', true,  false, array['player', 'group'],              true,  1),
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              false, 2),
    ('class',       'Class',       'multi',  false, true,  array['player', 'group'],              true,  3),
    ('format',      'Format',      'multi',  false, false, array['player', 'group', 'community'], false, 4),
    ('server-type', 'Server type', 'multi',  false, false, array['player', 'group', 'community'], false, 5),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true,  6)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'tf2';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('division', 'fresh-newcomer',   'Fresh / Newcomer',     1),
    ('division', 'open-amateur',     'Open / Amateur',       2),
    ('division', 'low-intermediate', 'Low / Intermediate',   3),
    ('division', 'mid-main',         'Mid / Main',           4),
    ('division', 'high-advanced',    'High / Advanced',      5),
    ('division', 'prem-invite',      'Premiership / Invite', 6),

    ('rank', 'mercenary-i',           'Mercenary I',           1),
    ('rank', 'mercenary-ii',          'Mercenary II',          2),
    ('rank', 'mercenary-iii',         'Mercenary III',         3),
    ('rank', 'contract-killer-i',     'Contract Killer I',     4),
    ('rank', 'contract-killer-ii',    'Contract Killer II',    5),
    ('rank', 'contract-killer-iii',   'Contract Killer III',   6),
    ('rank', 'executioner-i',         'Executioner I',         7),
    ('rank', 'executioner-ii',        'Executioner II',        8),
    ('rank', 'executioner-iii',       'Executioner III',       9),
    ('rank', 'expert-assassin-i',     'Expert Assassin I',     10),
    ('rank', 'expert-assassin-ii',    'Expert Assassin II',    11),
    ('rank', 'expert-assassin-iii',   'Expert Assassin III',   12),
    ('rank', 'death-merchant',        'Death Merchant',        13),

    ('class', 'scout',    'Scout',    1),
    ('class', 'soldier',  'Soldier',  2),
    ('class', 'pyro',     'Pyro',     3),
    ('class', 'demoman',  'Demoman',  4),
    ('class', 'heavy',    'Heavy',    5),
    ('class', 'engineer', 'Engineer', 6),
    ('class', 'medic',    'Medic',    7),
    ('class', 'sniper',   'Sniper',   8),
    ('class', 'spy',      'Spy',      9),

    ('format', 'sixes',      'Sixes (6v6)',      1),
    ('format', 'highlander', 'Highlander (9v9)', 2),
    ('format', 'ultiduo',    'Ultiduo (2v2)',    3),

    ('server-type', 'vanilla',      'Vanilla',            1),
    ('server-type', 'saxton-hale',  'Versus Saxton Hale', 2),
    ('server-type', 'jump',         'Jump',               3),
    ('server-type', 'surf',         'Surf',               4),
    ('server-type', 'mge',          'MGE',                5),
    ('server-type', 'dodgeball',    'Dodgeball',          6),
    ('server-type', 'trade',        'Trade',              7),
    ('server-type', 'custom',       'Custom game modes',  8),

    ('looking-for', 'casual',            'Casual',            1),
    ('looking-for', 'community-servers', 'Community servers', 2),
    ('looking-for', 'competitive',       'Competitive',       3),
    ('looking-for', 'mann-vs-machine',   'Mann vs. Machine',  4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'tf2';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('steam', 'logs.tf',   'https://logs.tf/profile/'),
    ('steam', 'demos.tf',  'https://demos.tf/profiles/'),
    ('steam', 'etf2l.org', 'https://etf2l.org/search/'),
    ('steam', 'rgl.gg',    'https://rgl.gg/Public/PlayerProfile.aspx?p=')
) as tracker (contact_kind, title, template)
where game.handle = 'tf2';
