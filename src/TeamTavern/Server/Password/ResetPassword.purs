module TeamTavern.Server.Password.ResetPassword (resetPassword) where

import Prelude

import Async (Async)
import Data.Variant (Variant)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, NotFoundRow_, noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Password.ResetPassword as ResetPassword
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound, queryNone, transaction)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Password.Infrastructure.DeleteResets (deleteResets)
import TeamTavern.Server.Player.Domain.Hash (Hash, generateHash)
import TeamTavern.Server.Player.Domain.Password (validatePassword')
import TeamTavern.Server.Session.Domain.Token (Token(..))
import TeamTavern.Server.Session.Domain.Token as Token
import Type.Row (type (+))

-- A link holds only while the account signs in with a password. One that has
-- moved to Discord, Steam or Google since isn't the email's to take back. A
-- second reset with the same link, racing this one, waits on the deleted row
-- and then finds none.
nonceQueryString :: Query
nonceQueryString = Query """
    with used as (
        delete from password_reset
        where nonce_hash = $1
        returning player_id, created
    )
    select player.id as "playerId"
    from used
    join player on player.id = used.player_id
    where used.created > now() - interval '1 hour'
        and player.password_hash is not null
    """

ensureValidNonce :: forall errors querier. Querier querier =>
    querier -> String -> Async (LoadSingleError errors) Int
ensureValidNonce querier nonce = do
    nonceHash <- Token.hash $ Token nonce
    {playerId} :: {playerId :: Int} <-
        queryFirstNotFound querier nonceQueryString (nonceHash : [])
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

-- Whoever else holds a session of the player's, perhaps the one the reset is
-- meant to lock out, has it ended with the old password.
sessionsQueryString :: Query
sessionsQueryString = Query """
    update session
    set revoked = true
    where session.player_id = $1
    """

revokeSessions :: forall querier errors. Querier querier =>
    querier -> Int -> Async (InternalTerror_ errors) Unit
revokeSessions querier playerId =
    queryNone querier sessionsQueryString (playerId : [])

resetPassword :: forall left.
    Pool -> ResetPassword.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow ResetPassword.BadContent + NotFoundRow_ + InternalRow_ + ()))
resetPassword pool {password, nonce} =
    sendResponse "Error resetting password" do
    -- Validate password.
    validPassword <- validatePassword' password

    -- Generate password hash.
    hash <- generateHash validPassword

    pool # transaction \client -> do
        -- Ensure nonce is valid.
        playerId <- ensureValidNonce client nonce

        -- Update the password and end every session and other link.
        updatePassword client playerId hash
        revokeSessions client playerId
        deleteResets client playerId

    pure noContent_
