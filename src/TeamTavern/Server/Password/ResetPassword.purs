module TeamTavern.Server.Password.ResetPassword (resetPassword) where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Password.ResetPassword as ResetPassword
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Hash (Hash, generateHash)
import TeamTavern.Server.Player.Domain.Password (validatePassword')

nonceQueryString :: Query
nonceQueryString = Query """
    update password_reset
    set consumed = true
    where password_reset.nonce = $1
    and password_reset.consumed = false
    and extract(epoch from (now() - password_reset.created)) < 3600 -- 1 hour
    returning password_reset.player_id as "playerId"
    """

ensureValidNonce :: forall errors querier. Querier querier =>
    querier -> String -> Async (LoadSingleError errors) Int
ensureValidNonce querier nonce = do
    {playerId} :: {playerId :: Int} <-
        queryFirstNotFound querier nonceQueryString (nonce : [])
    pure playerId

passwordQueryString :: Query
passwordQueryString = Query """
    update player
    set password_hash = $2
    where player.id = $1
    """

updatePassword :: forall querier errors. Querier querier =>
    querier -> Int -> Hash -> Async (InternalTerror_ errors) Unit
updatePassword querier playerId hash =
    queryNone querier passwordQueryString (playerId :| hash)

resetPassword :: forall left.
    Pool -> Cookies  -> ResetPassword.RequestContent -> Async left _
resetPassword pool cookies {password, nonce} =
    sendResponse "Error resetting password" do
    -- Ensure user is not signed in.
    ensureNotSignedIn cookies

    -- Validate password.
    validPassword <- validatePassword' password

    -- Ensure nonce is valid.
    playerId <- ensureValidNonce pool nonce

    -- Generate password hash.
    hash <- generateHash validPassword

    -- Update the password.
    updatePassword pool playerId hash

    pure noContent_
