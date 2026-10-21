module TeamTavern.Server.Session.Start.CheckPassword
    (CheckPasswordError, CheckedPassword, checkPassword, confirmPassword) where

import Prelude

import Async (Async, left)
import Bcrypt.Async as Bcrypt
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, isNothing)
import Data.Variant (Variant, inj)
import Jarilo (InternalRow_, BadRequestRow, badRequest_, internal__)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst, queryFirstMaybe)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type CheckPasswordError errors = TerrorVar
    ( InternalRow_
    + BadRequestRow StartSession.BadContent
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

checkPassword
    :: ∀ querier errors
    .  Querier querier
    => StartSession.RequestContentEmail
    -> querier
    -> Async (CheckPasswordError errors) CheckedPassword
checkPassword model querier = do
    -- Load player hash.
    checked @ { nickname, hash } :: CheckedPassword <-
        queryFirst unknownPlayer querier queryString (model.emailOrNickname : [])
        # lmapElaborate ("Can't find player: " <> model.emailOrNickname)

    -- Compare hash with password.
    matches <- Bcrypt.compare model.password hash # lmap
        (print >>> ("Bcrypt error while checking hash: " <> _) >>> singleton >>> Terror internal__)
    when (not matches) $ left $ Terror wrongPassword [ "Wrong password entered for user: " <> nickname ]

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
