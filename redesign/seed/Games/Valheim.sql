-- Valheim
--
-- Server type leads the card: what a Valheim post plays on is the first thing
-- that rules it in or out, and a community answers it about itself as a player
-- and a group do, which is why nothing here says what kind of thing a community
-- is (brief 5, 5.3).
--
-- Valheim has no rank and no roles. The game sorts nobody by skill and gives
-- nobody a part to play, so what a post is looking for carries the card alone.
--
-- Crossplay reaches all four platforms, but a modded world runs on Steam alone,
-- so what a post plays on decides who can join it.

insert into game (title, short_title, handle, description)
values
    ( 'Valheim'
    , 'Valheim'
    , 'valheim'
    , array['Find Valheim players, groups and communities: someone to start a world with, or a server to settle on.']
    );

-- Valheim knows its players by the platform each one plays on: crossplay is
-- join codes rather than an account the game keeps, so a post offers the id its
-- own platform gives it.

insert into game_contact (game_id, kind)
select game.id, contact.kind
from game
cross join (values ('discord'), ('steam'), ('psn'), ('gamer_tag'), ('friend_code')) as contact (kind)
where game.handle = 'valheim';

insert into field (game_id, key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
select game.id, field.key, field.label, field.ilk, field.ordered, field.slotted, field.applies_to, field.on_card, field.ordinal
from game
cross join (values
    ('server-type',       'Server type',       'multi', false, false, array['player', 'group', 'community'], true,  1),
    ('platform',          'Platform',          'multi', false, false, array['player', 'group', 'community'], true,  2),
    ('looking-for',       'Looking for',       'multi', false, false, array['player', 'group', 'community'], true,  3),
    ('server-characters', 'Server characters', 'multi', false, false, array['player', 'group', 'community'], false, 4)
) as field (key, label, ilk, ordered, slotted, applies_to, on_card, ordinal)
where game.handle = 'valheim';

insert into field_option (field_id, key, label, ordinal)
select field.id, option.key, option.label, option.ordinal
from field
join game on game.id = field.game_id
join (values
    ('server-type', 'vanilla', 'Vanilla', 1),
    ('server-type', 'modded',  'Modded',  2),

    ('platform', 'pc',          'PC',          1),
    ('platform', 'playstation', 'PlayStation', 2),
    ('platform', 'xbox',        'Xbox',        3),
    ('platform', 'switch',      'Switch',      4),

    ('looking-for', 'casual',   'Casual',   1),
    ('looking-for', 'building', 'Building', 2),
    ('looking-for', 'events',   'Events',   3),
    ('looking-for', 'roleplay', 'Roleplay', 4),

    ('server-characters', 'new-characters',      'New characters',      1),
    ('server-characters', 'existing-characters', 'Existing characters', 2)
) as option (field_key, key, label, ordinal) on option.field_key = field.key
where game.handle = 'valheim';
