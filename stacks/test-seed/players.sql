-- The accounts and posts the specs drive the site with. Every account signs in
-- with the password `tester-password`, whose bcrypt hash this is, and is
-- confirmed, so the email features have someone to send to.
--
-- Every account gives the same facts and fills every contact column, so
-- whichever contacts a game offers, its cards have some to show. A post's
-- renewal nonce is derived rather than random, so a boot gives the same one.

create function seed_player(nickname text, email text) returns integer
language sql as $$
    insert into player
        ( nickname, email, email_confirmed, password_hash
        , birthday, languages, country, timezone
        , discord_tag, steam_id, riot_id, battle_tag, ea_id
        , ubisoft_username, psn_id, gamer_tag, friend_code
        )
    values
        ( nickname, email, true, '$2b$10$.ooPKTLO.JoL61KIvfsTKu2Nx1awadTkA9C1h/29.mIbi86dhHFwO'
        , date '2000-06-15', array['English'], 'Croatia', 'Europe/Zagreb'
        , nickname, nickname, nickname || '#EUW', nickname || '#1234', nickname
        , nickname, nickname, nickname, 'SW-1234-5678-9012'
        )
    returning id
$$;

-- One tester per game with a player post, derived from the game table, so a
-- new file in Seed/Games gets one without an edit here. The nickname is the
-- handle title-cased with its hyphens dropped and Tester after it, so
-- `apex-legends` gets `ApexLegendsTester`, and the email is
-- `apex-legends@example.com`.
--
-- The post answers every field a player is asked: a single field with its
-- middle option, so a rank lands mid-ladder; a multi field with its first; a
-- boolean with yes.

with tester as (
    select
        game.id as game_id,
        game.title,
        replace(initcap(game.handle), '-', '') || 'Tester' as nickname,
        game.handle || '@example.com' as email
    from game
),
account as (
    select tester.*, seed_player(tester.nickname, tester.email) as player_id
    from tester
),
posted as (
    insert into post
        ( player_id, game_id, ilk, renewal_nonce, summary
        , microphone, online_from, online_to, contact_preference
        )
    select
        account.player_id,
        account.game_id,
        'player',
        left(md5(account.nickname || '-player'), 20),
        array['Seeded player post for ' || account.title || '.'],
        true,
        time '19:00',
        time '23:00',
        'either'
    from account
    returning id, game_id
),
flagged as (
    insert into post_field_flag (post_id, field_id)
    select posted.id, field.id
    from posted
    join field on field.game_id = posted.game_id
        and field.ilk = 'boolean' and 'player' = any(field.applies_to)
)
insert into post_field_option (post_id, field_option_id)
select posted.id, chosen.id
from posted
join field on field.game_id = posted.game_id
    and field.ilk <> 'boolean' and 'player' = any(field.applies_to)
cross join lateral (
    select option.id
    from field_option option
    cross join (select (max(ordinal) + 1) / 2 as middle from field_option where field_id = field.id) ladder
    where option.field_id = field.id
    order by case when field.ilk = 'single' then abs(option.ordinal - ladder.middle) else option.ordinal end
    limit 1
) chosen;

-- The hand-written posts below answer their fields by key. A post is found by
-- its owner and type, since a player has one post of each type per game and
-- the accounts that use them post in one game alone.

create function seed_post_option(nickname text, ilk text, field_key text, option_keys text[]) returns void
language sql as $$
    insert into post_field_option (post_id, field_option_id)
    select post.id, option.id
    from post
    join player on player.id = post.player_id
    join field on field.game_id = post.game_id and field.key = field_key
    join field_option option on option.field_id = field.id and option.key = any(option_keys)
    where player.nickname = seed_post_option.nickname and post.ilk = seed_post_option.ilk
$$;

create function seed_post_range(nickname text, ilk text, field_key text, from_key text, to_key text) returns void
language sql as $$
    insert into post_field_range (post_id, field_id, from_option_id, to_option_id)
    select
        post.id,
        field.id,
        (select id from field_option where field_id = field.id and key = from_key),
        (select id from field_option where field_id = field.id and key = to_key)
    from post
    join player on player.id = post.player_id
    join field on field.game_id = post.game_id and field.key = field_key
    where player.nickname = seed_post_range.nickname and post.ilk = seed_post_range.ilk
$$;

-- Valorant carries every post type, so its feed mixes all three: a second
-- account owns a group post and a community post, both modelled on the
-- prototype's fixtures.

select seed_player('GroupTester', 'group@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, age_from
    , group_size, group_wanted_from, group_wanted_to
    )
select
    player.id,
    game.id,
    'group',
    left(md5('GroupTester-group'), 20),
    array['Three friends who play most nights, we want to stop solo queuing for the last two spots. No tilt, comms on, we review our losses on Sundays.'],
    true,
    time '21:00',
    time '01:00',
    'message',
    'Night Owls',
    array['Europe'],
    array['English'],
    18,
    3,
    2,
    2
from player, game
where player.nickname = 'GroupTester' and game.handle = 'valorant';

select
    seed_post_range('GroupTester', 'group', 'rank', 'platinum-1', 'diamond-3'),
    seed_post_option('GroupTester', 'group', 'role', array['controller', 'sentinel']),
    seed_post_option('GroupTester', 'group', 'platform', array['pc']),
    seed_post_option('GroupTester', 'group', 'looking-for', array['ranked']);

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, age_from, discord_server, website
    )
select
    player.id,
    game.id,
    'community',
    left(md5('GroupTester-community'), 20),
    array['An EU Valorant community of about 400 players. We run in-house 10-mans every Friday, a monthly cup with small prizes, and coaching nights where our Immortal and Radiant members review your VODs. Find a duo in #lfg, join a scrim team, or just hang out in voice.'],
    true,
    time '18:00',
    time '01:00',
    'discord',
    'Radiant Rising',
    array['Europe'],
    array['English'],
    16,
    'https://discord.gg/radiantrising',
    'https://radiantrising.example.com'
from player, game
where player.nickname = 'GroupTester' and game.handle = 'valorant';

select
    seed_post_option('GroupTester', 'community', 'platform', array['pc']),
    seed_post_option('GroupTester', 'community', 'looking-for', array['casual', 'ranked']);

-- An account with no post, for what the site shows a player who has none.

select seed_player('NewTester', 'new@example.com');

-- A Valorant player post past its 30 days, so the feed's divider has something
-- under it and the owner's pages have an expired post to renew.

select seed_player('ExpiredTester', 'expired@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , created, updated
    )
select
    player.id,
    game.id,
    'player',
    left(md5('ExpiredTester-player'), 20),
    array['Seeded player post that has expired.'],
    false,
    time '22:30',
    time '02:00',
    'either',
    current_timestamp - interval '45 days',
    current_timestamp - interval '45 days'
from player, game
where player.nickname = 'ExpiredTester' and game.handle = 'valorant';

select
    seed_post_option('ExpiredTester', 'player', 'rank', array['gold-1']),
    seed_post_option('ExpiredTester', 'player', 'role', array['duelist']),
    seed_post_option('ExpiredTester', 'player', 'platform', array['pc']),
    seed_post_option('ExpiredTester', 'player', 'looking-for', array['casual']);

-- Two sessions of ExpiredTester's with known tokens, one last used within the
-- year a session lasts and one before it.
insert into session (player_id, token_hash, generated, last_used)
select player.id, encode(sha256(convert_to(token, 'UTF8')), 'hex'), last_used, last_used
from player, (values
    ('11111111111111111111111111111111111111ab', current_timestamp - interval '11 months'),
    ('11111111111111111111111111111111111111cd', current_timestamp - interval '13 months')
) as idle (token, last_used)
where player.nickname = 'ExpiredTester';

-- A Counter-Strike 2 group that wants a player who can lead, for how an
-- in-game leader fits against a group that asks for one. It is in CS2 so that
-- Valorant's feed, which the specs assert whole, stays as it is.

select seed_player('LeaderlessTester', 'leaderless@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, age_from
    , group_size, group_wanted_from, group_wanted_to
    )
select
    player.id,
    game.id,
    'group',
    left(md5('LeaderlessTester-group'), 20),
    array['Four of us grinding Premier, and nobody wants to call. Looking for someone who can run the mid-round and keep the comms calm.'],
    true,
    time '20:00',
    time '00:00',
    'message',
    'Last Call',
    array['Europe'],
    array['English'],
    18,
    4,
    1,
    1
from player, game
where player.nickname = 'LeaderlessTester' and game.handle = 'counter-strike-2';

select
    seed_post_range('LeaderlessTester', 'group', 'premier-rating', '10k', '14k'),
    seed_post_option('LeaderlessTester', 'group', 'looking-for', array['ranked']);

insert into post_field_flag (post_id, field_id)
select post.id, field.id
from post
join player on player.id = post.player_id
join field on field.game_id = post.game_id and field.key = 'in-game-leader'
where player.nickname = 'LeaderlessTester' and post.ilk = 'group';

-- An owner with a post in each state, for the home page: a group that is
-- active, a player post in its last week and one expired. They are in games
-- whose feeds no spec asserts, so renewing one moves nothing another spec reads.
-- The posts answer none of their games' fields.

select seed_player('OwnerTester', 'owner@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, age_from
    , group_size, group_wanted_from, group_wanted_to
    )
select
    player.id,
    game.id,
    'group',
    left(md5('OwnerTester-group'), 20),
    array['Two of us queue most evenings and want a steady trio for ranked.'],
    true,
    time '19:00',
    time '23:00',
    'message',
    'Kestrel''s Nest',
    array['Europe'],
    array['English'],
    18,
    2,
    1,
    1
from player, game
where player.nickname = 'OwnerTester' and game.handle = 'dota-2';

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , created, updated
    )
select
    player.id,
    game.id,
    'player',
    left(md5('OwnerTester-player-' || game.handle), 20),
    array['Seeded player post of OwnerTester''s.'],
    true,
    time '20:00',
    time '23:30',
    'either',
    current_timestamp - posted.age,
    current_timestamp - posted.age
from player, game
join (values
    ('heroes-of-the-storm', interval '45 days'),
    ('valheim', interval '26 days')
) as posted (handle, age) on posted.handle = game.handle
where player.nickname = 'OwnerTester';

-- The post in its last week has had its notice that it is expiring, as the
-- period worker gives one, so the bell's list has an expiry row.

insert into notification (post_id, kind)
select post.id, 'expiry'
from post
join player on player.id = post.player_id
join game on game.id = post.game_id
where player.nickname = 'OwnerTester' and game.handle = 'valheim';

-- Every way a post asks to be reached, for the contact panel. Team Fortress
-- 2's tester would rather be added off-site, and a community there is joined
-- through its website; Valorant carries the other four.

update post
set contact_preference = 'offsite'
from player
where player.id = post.player_id and player.nickname = 'TeamFortress2Tester';

select seed_player('CommunityTester', 'community@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, website
    )
select
    player.id,
    game.id,
    'community',
    left(md5('CommunityTester-community'), 20),
    array['A payload server community with a pub night every Thursday and a league team for anyone who wants one.'],
    true,
    time '19:00',
    time '23:00',
    'website',
    'Payload Pals',
    array['Europe'],
    array['English'],
    'payloadpals.example.com'
from player, game
where player.nickname = 'CommunityTester' and game.handle = 'team-fortress-2';

-- An owner with two expired posts to renew, one from the feed and one from
-- its page, in games whose feeds no other spec asserts.

select seed_player('RenewTester', 'renew@example.com');

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , created, updated
    )
select
    player.id,
    game.id,
    'player',
    left(md5('RenewTester-player-' || game.handle), 20),
    array['Seeded player post of RenewTester''s.'],
    true,
    time '20:00',
    time '23:00',
    'either',
    current_timestamp - interval '40 days',
    current_timestamp - interval '40 days'
from player, game
where player.nickname = 'RenewTester' and game.handle in ('rainbow-six-siege', 'overwatch');

-- Owners whose mail the email specs read: MailTester's with an active post and
-- an expired one, whose message email carries Renew, and QuietTester's with
-- message emails switched off. Their games' feeds are read by card, never whole.

select seed_player('MailTester', 'mail@example.com');
select seed_player('QuietTester', 'quiet@example.com');

update player set email_messages = false where nickname = 'QuietTester';

insert into post
    ( player_id, game_id, ilk, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , created, updated
    )
select
    player.id,
    game.id,
    'player',
    left(md5(player.nickname || '-player-' || game.handle), 20),
    array['Seeded player post of ' || player.nickname || '''s.'],
    true,
    time '20:00',
    time '23:00',
    'either',
    current_timestamp - posted.age,
    current_timestamp - posted.age
from player
join (values
    ('MailTester', 'counter-strike-2', interval '0 days'),
    ('MailTester', 'overwatch', interval '40 days'),
    ('QuietTester', 'counter-strike-2', interval '0 days')
) as posted (nickname, handle, age) on posted.nickname = player.nickname
join game on game.handle = posted.handle;

drop function seed_post_range(text, text, text, text, text);
drop function seed_post_option(text, text, text, text[]);
drop function seed_player(text, text);
