module TeamTavern.Server.Session.Start.CheckPassword (CheckPasswordError, checkPassword) where

import Prelude

import Async (Async, left)
import Bcrypt.Async as Bcrypt
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Variant (inj)
import Jarilo (InternalRow_, BadRequestRow, badRequest_, internal__)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type CheckPasswordError errors = TerrorVar
    ( InternalRow_
    + BadRequestRow StartSession.BadContent
    + errors
    )

queryString :: Query
queryString = Query """
    select
        player.id,
        player.nickname,
        player.password_hash as hash
    from player
    where lower(player.email) = lower($1)
        or lower(player.nickname) = lower($1)
    """

checkPassword
    :: ∀ querier errors
    .  Querier querier
    => StartSession.RequestContent
    -> querier
    -> Async (CheckPasswordError errors) { id :: Int, nickname :: String }
checkPassword model querier = do
    let unknownPlayer = badRequest_ $ inj (Proxy :: _ "unknownPlayer") {}
    let wrongPassword = badRequest_ $ inj (Proxy :: _ "wrongPassword") {}
    -- Load player hash.
    { id, nickname, hash } :: { id :: Int, nickname :: String, hash :: String } <-
        queryFirst unknownPlayer querier queryString (model.emailOrNickname : [])
        # lmapElaborate ("Can't find player: " <> model.emailOrNickname)

    -- Compare hash with password.
    matches <- Bcrypt.compare model.password hash # lmap
        (print >>> ("Bcrypt error while checking hash: " <> _) >>> singleton >>> Terror internal__)
    when (not matches) $ left $ Terror wrongPassword [ "Wrong password entered for user: " <> nickname ]

    pure $ { id, nickname }
