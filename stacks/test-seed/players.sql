-- One player with one profile for every seeded game, so that every listing page
-- has a row to assert on. Derived from the game table rather than written out
-- per game, so a new file in Seed/Games gets a profile without an edit here.
-- Every player needs a sign-in identity, so each signs in with the password
-- `tester-password`, whose bcrypt hash this is.

with tester as (
    select
        game.id as game_id,
        game.title,
        game.platforms[1] as platform,
        game.handle || '@example.com' as email,
        initcap(game.handle) || 'Tester' as nickname
    from game
),
inserted as (
    insert into player (email, nickname, password_hash, languages, location, microphone)
    select
        email,
        nickname,
        '$2b$10$.ooPKTLO.JoL61KIvfsTKu2Nx1awadTkA9C1h/29.mIbi86dhHFwO',
        array['English'],
        'Croatia',
        true
    from tester
    returning id, nickname
)
insert into player_profile (player_id, game_id, platform, new_or_returning, about, ambitions)
select
    inserted.id,
    tester.game_id,
    tester.platform,
    false,
    array['Seeded profile for ' || tester.title || '.'],
    array['Find a team to play ' || tester.title || ' with.']
from tester
join inserted on inserted.nickname = tester.nickname;
