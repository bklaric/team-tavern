module TeamTavern.Server.Player.UpdatePlayerEmail (checkPassword, updatePlayerEmail) where

import Prelude

import Async (Async, left)
import Bcrypt.Async as Bcrypt
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (inj)
import Jarilo (badRequest_, internal__, noContent_)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (execute)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Player.UpdatePlayerEmail as UpdatePlayerEmail
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Error (Terror(..), lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines, queryFirst, queryFirstInternal, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail')
import TeamTavern.Server.Player.Domain.Id (Id)
import Type.Proxy (Proxy(..))

comparePassword :: String -> Maybe String -> String -> Async _ Unit
comparePassword nickname password hash = do
    let wrongPassword = badRequest_ $ inj (Proxy :: _ "wrongPassword") {}
    case password of
        Nothing -> left $ Terror wrongPassword
            ["No password entered for user: " <> nickname]
        Just password' -> do
            matches <- Bcrypt.compare password' hash # lmap \error ->
                Terror internal__ ["Bcrypt error while checking hash: " <> print error]
            when (not matches) $ left $ Terror wrongPassword
                ["Wrong password entered for user: " <> nickname]

passwordQueryString :: Query
passwordQueryString = Query """
    select player.password_hash as hash
    from player
    where lower(player.nickname) = lower($1)
        and player.password_hash is not null
    """

checkPassword :: ∀ querier. Querier querier =>
    String -> String -> querier -> Async _ Unit
checkPassword nickname password querier = do
    let wrongPassword = badRequest_ $ inj (Proxy :: _ "wrongPassword") {}

    -- Load player hash.
    {hash} :: {hash :: String} <-
        queryFirst wrongPassword querier passwordQueryString (nickname : [])
        # lmapElaborate ("Can't find player with a password: " <> nickname)

    comparePassword nickname (Just password) hash

identityQueryString :: Query
identityQueryString = Query """
    select player.password_hash as hash
    from player
    where player.id = $1
    """

-- | A player with a password confirms the change with it. A Discord player has
-- | none to give, and being signed in is all that is asked.
checkIdentity :: ∀ querier. Querier querier =>
    Id -> String -> Maybe String -> querier -> Async _ Unit
checkIdentity id nickname password querier = do
    {hash} :: {hash :: Maybe String} <- queryFirstInternal querier identityQueryString (id : [])
    case hash of
        Nothing -> pure unit
        Just hash' -> comparePassword nickname password hash'

emailQueryString :: Query
emailQueryString = Query """
    update player
    set email = $2
    where id = $1
    """

updateEmail :: forall querier. Querier querier =>
    Id -> Email -> querier -> Async _ Unit
updateEmail id email querier = do
    querier # execute emailQueryString (id :| email) # lmap \error ->
        case code error == unique_violation of
        true | constraint error == Just "player_lower_email_key"
            -> Terror
                (badRequest_ $ inj (Proxy :: _ "emailTaken") {})
                ["Player email is taken: " <> show email, print error]
        _ -> Terror internal__ $ databaseErrorLines error

updatePlayerEmail :: ∀ left.
    Pool -> String -> Cookies -> UpdatePlayerEmail.RequestContent -> Async left _
updatePlayerEmail pool nickname cookies body =
    sendResponse "Error updating player email" do
    -- Read requestor info from cookies.
    {id} <- ensureSignedInAs pool cookies nickname

    -- Validate email.
    email <- validateEmail' body.email

    pool # transaction \client -> do
        -- Make sure the password is correct, if the player has one.
        checkIdentity id nickname body.password client

        -- Update email.
        updateEmail id email client

    pure $ noContent_
