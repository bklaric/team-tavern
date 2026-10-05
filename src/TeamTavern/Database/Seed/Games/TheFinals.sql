-- The Finals
--
-- Ranks carry their divisions, "Gold 4" as the game writes them, so a rank
-- range spans divisions and rank closeness counts in steps of one (brief 7.2).
-- Divisions are numbered 4 up to 1, so the ordinals run Bronze 4, Bronze 3,
-- Bronze 2, Bronze 1, Silver 4 and on: worst to best, the reverse of the
-- numbering inside each tier. Ruby, the season's top 500, has no divisions.
-- Players give their Rank Score as often as their league, "35k", and every
-- division is 2,500 of it, so a score falls in exactly one option.
-- World Tour's leagues are not a rank: every mode advances them and a loss
-- never takes points away, so they say how much someone plays, not how well.
--
-- Class is Light, Medium or Heavy, what players say they main and ask for: a
-- Light main looks for a regular Medium and Heavy. It is slotted: two players
-- fit by covering two different ones. A team may field two of a class, but a
-- post asks for the class it lacks. Players don't name a shotcaller, so there
-- is no in-game leader field.
--
-- Cashout is a tournament in the game's own words, ranked or not, but it is a
-- queue, so a post after it is Casual or Ranked. Scrims and tournaments is
-- organised play outside the queues. The Grand Major, Embark's yearly event
-- with open qualifiers, is part of it rather than a team format of its own,
-- since posts seldom recruit for it by name.
--
-- There is no server field: posts name the region they play in, which the
-- post's own regions field says.
--
-- The Finals runs on PC, PlayStation and Xbox, with crossplay everywhere,
-- ranked included, unless a player turns it off. What a post plays on decides
-- which account it offers and whether a party can use its console's own chat,
-- so it is still a field here.
--
-- The friend list is Embark's on every platform, so players add each other by
-- Embark ID, and console players by their PSN ID or gamertag too. Steam is not
-- offered: PC players add each other through the same Embark friend list.

insert into game (title, short_title, handle, description)
values
    ( 'The Finals'
    , 'The Finals'
    , 'the-finals'
    , array['Find The Finals players, groups and communities: a third for ranked, a regular squad for Quick Cash, a team for tournaments, or a club to join.']
    );

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('embark'), ('psn'), ('gamer_tag')) as contact (kind)
where game.handle = 'the-finals';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('rank',        'Rank',        'single', true,  false, array['player', 'group'],              true, 1),
    ('class',       'Class',       'multi',  false, true,  array['player', 'group'],              true, 2),
    ('platform',    'Platform',    'multi',  false, false, array['player', 'group', 'community'], true, 3),
    ('looking-for', 'Looking for', 'multi',  false, false, array['player', 'group', 'community'], true, 4)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'the-finals';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('rank', 'bronze-4',   'Bronze 4',   1),
    ('rank', 'bronze-3',   'Bronze 3',   2),
    ('rank', 'bronze-2',   'Bronze 2',   3),
    ('rank', 'bronze-1',   'Bronze 1',   4),
    ('rank', 'silver-4',   'Silver 4',   5),
    ('rank', 'silver-3',   'Silver 3',   6),
    ('rank', 'silver-2',   'Silver 2',   7),
    ('rank', 'silver-1',   'Silver 1',   8),
    ('rank', 'gold-4',     'Gold 4',     9),
    ('rank', 'gold-3',     'Gold 3',     10),
    ('rank', 'gold-2',     'Gold 2',     11),
    ('rank', 'gold-1',     'Gold 1',     12),
    ('rank', 'platinum-4', 'Platinum 4', 13),
    ('rank', 'platinum-3', 'Platinum 3', 14),
    ('rank', 'platinum-2', 'Platinum 2', 15),
    ('rank', 'platinum-1', 'Platinum 1', 16),
    ('rank', 'diamond-4',  'Diamond 4',  17),
    ('rank', 'diamond-3',  'Diamond 3',  18),
    ('rank', 'diamond-2',  'Diamond 2',  19),
    ('rank', 'diamond-1',  'Diamond 1',  20),
    ('rank', 'ruby',       'Ruby',       21),

    ('class', 'light',  'Light',  1),
    ('class', 'medium', 'Medium', 2),
    ('class', 'heavy',  'Heavy',  3),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),

    ('looking-for', 'casual',             'Casual',                 1),
    ('looking-for', 'ranked',             'Ranked',                 2),
    ('looking-for', 'scrims-tournaments', 'Scrims and tournaments', 3),
    ('looking-for', 'learning-the-game',  'Learning the game',      4)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'the-finals';

insert into tracker (game_id, contact_kind, title, template)
select game.id, tracker.contact_kind, tracker.title, tracker.template
from game
cross join (values
    ('embark', 'thefinals.arenyze.com', 'https://thefinals.arenyze.com/player/')
) as tracker (contact_kind, title, template)
where game.handle = 'the-finals';
