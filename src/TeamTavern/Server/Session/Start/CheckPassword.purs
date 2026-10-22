module TeamTavern.Server.Session.Start.CheckPassword
    (CheckPasswordError, CheckedPassword, SignInLimits, checkPassword, confirmPassword, createSignInLimits) where

import Prelude

import Async (Async, fromEffect, left)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, isNothing)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Jarilo (InternalRow_, BadRequestRow, TooManyRequestsRow, badRequest_, internal__)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt, comparePassword)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst, queryFirstMaybe)
import TeamTavern.Server.Infrastructure.RateLimit (ClientLimits, Limiter, clear, createClientLimiter, createLimiter, ensureAdmitted, release)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type CheckPasswordError errors = TerrorVar
    ( InternalRow_
    + BadRequestRow StartSession.BadContent
    + TooManyRequestsRow Wait
    + errors
    )

type CheckedPassword = { id :: Int, nickname :: String, hash :: String }

unknownPlayer :: ∀ errors. Variant (BadRequestRow StartSession.BadContent errors)
unknownPlayer = badRequest_ $ inj (Proxy :: _ "unknownPlayer") {}

wrongPassword :: ∀ errors. Variant (BadRequestRow StartSession.BadContent errors)
wrongPassword = badRequest_ $ inj (Proxy :: _ "wrongPassword") {}

queryString :: Query
queryString = Query """
    select
        player.id,
        player.nickname,
        player.password_hash as hash
    from player
    where player.password_hash is not null
        and (lower(player.email) = lower($1) or lower(player.nickname) = lower($1))
    """

confirmString :: Query
confirmString = Query """
    select 1 as one
    from player
    where id = $1 and password_hash = $2
    for update
    """

-- | Passwords can be guessed, so a client gets 20 failed sign-ins a quarter
-- | of an hour, and an account 10 wrong passwords, after which it takes none,
-- | the right one included, until the quarter is over or the password is
-- | reset. Only failures count, and an account is counted by its id once
-- | it's found. The right password clears its account's failures.
type SignInLimits = { perClient :: Limiter, perAccount :: Limiter }

createSignInLimits :: ClientLimits -> Effect SignInLimits
createSignInLimits clientLimits = do
    perClient <- createClientLimiter clientLimits "Failed sign-ins from one client" { count: 20, seconds: 900 }
    perAccount <- createLimiter "Failed sign-ins to one account" { count: 10, seconds: 900 }
    pure { perClient, perAccount }

checkPassword
    :: ∀ querier errors
    .  Querier querier
    => SignInLimits
    -> Bcrypt
    -> String
    -> StartSession.RequestContentEmail
    -> querier
    -> Async (CheckPasswordError errors) CheckedPassword
checkPassword limits bcrypt client model querier = do
    -- Each try counts as a failure until the password matches, so tries sent
    -- at once can't all find room while bcrypt runs.
    ensureAdmitted limits.perClient client "Too many failed sign-ins from one client."

    -- Load player hash.
    checked @ { id, nickname, hash } :: CheckedPassword <-
        queryFirst unknownPlayer querier queryString (model.emailOrNickname : [])
        # lmapElaborate ("Can't find player: " <> model.emailOrNickname)
    let account = show id
    ensureAdmitted limits.perAccount account "Too many failed sign-ins to one account."

    -- Compare hash with password.
    matches <- comparePassword bcrypt model.password hash # lmap
        (print >>> ("Bcrypt error while checking hash: " <> _) >>> singleton >>> Terror internal__)
    when (not matches) $ left $ Terror wrongPassword [ "Wrong password entered for user: " <> nickname ]

    fromEffect do
        release limits.perClient client
        clear limits.perAccount account
    pure checked

-- | Locks the player for the session's transaction and checks that the hash
-- | `checkPassword` matched is still theirs, so a reset or a switch to another
-- | sign-in that commits in between isn't undone by a session for the old
-- | password.
confirmPassword
    :: ∀ querier errors
    .  Querier querier
    => CheckedPassword
    -> querier
    -> Async (CheckPasswordError errors) Unit
confirmPassword { id, nickname, hash } querier = do
    confirmed :: Maybe { one :: Int } <- queryFirstMaybe querier confirmString (id :| hash)
    when (isNothing confirmed) $ left $ Terror wrongPassword [ "Password changed while signing in: " <> nickname ]
