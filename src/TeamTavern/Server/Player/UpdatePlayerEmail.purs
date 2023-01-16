module TeamTavern.Server.Player.UpdatePlayerEmail (checkPassword, updatePlayerEmail) where

import Prelude

import Async (Async, left)
import Bcrypt.Async as Bcrypt
import Data.Bifunctor (lmap)
import Data.Variant (inj)
import Jarilo (badRequest_, internal__, noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Player.UpdatePlayerEmail as UpdatePlayerEmail
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Error (Terror(..), lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst, queryNone, transaction)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail')
import TeamTavern.Server.Player.Domain.Id (Id)
import Type.Proxy (Proxy(..))

passwordQueryString :: Query
passwordQueryString = Query """
    select player.password_hash as hash
    from player
    where lower(player.nickname) = lower($1)
    """

checkPassword :: ∀ querier. Querier querier =>
    String -> String -> querier -> Async _ Unit
checkPassword nickname password querier = do
    let wrongPassword = badRequest_ $ inj (Proxy :: _ "wrongPassword") {}

    -- Load player hash.
    {hash} :: {hash :: String} <-
        queryFirst wrongPassword querier passwordQueryString (nickname : [])
        # lmapElaborate ("Can't find player: " <> nickname)

    -- Compare hash with password.
    matches <- Bcrypt.compare password hash # lmap \error ->
        Terror internal__ ["Bcrypt error while checking hash: " <> print error]
    when (not matches) $ left $ Terror wrongPassword
        ["Wrong password entered for user: " <> nickname]

emailQueryString :: Query
emailQueryString = Query """
    update player
    set email = $2
    where id = $1
    """

updateEmail :: forall errors querier. Querier querier =>
    Id -> Email -> querier -> Async (InternalTerror_ errors) Unit
updateEmail id email client =
    queryNone client emailQueryString (id :| email)

updatePlayerEmail :: ∀ left.
    Pool -> String -> Cookies -> UpdatePlayerEmail.RequestContent -> Async left _
updatePlayerEmail pool nickname cookies body =
    sendResponse "Error updating player email" do
    -- Read requestor info from cookies.
    {id} <- ensureSignedInAs pool cookies nickname

    -- Validate email.
    email <- validateEmail' body.email

    pool # transaction \client -> do
        -- Make sure the password is correct.
        checkPassword nickname body.password client

        -- Update email.
        updateEmail id email client

    pure $ noContent_
