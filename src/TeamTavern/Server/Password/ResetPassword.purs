module TeamTavern.Server.Password.ResetPassword (resetPassword) where

import Prelude

import Async (Async, fromEffect)
import Data.Variant (Variant)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, NotFoundRow_, noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Password.ResetPassword as ResetPassword
import TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound, queryNone, transaction)
import TeamTavern.Server.Infrastructure.RateLimit (clear)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Password.Infrastructure.DeleteResets (deleteResets)
import TeamTavern.Server.Player.Domain.Hash (Hash, generateHash)
import TeamTavern.Server.Player.Domain.Password (validatePassword')
import TeamTavern.Server.Session.Domain.Token (Token(..), TokenHash)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CheckPassword (SignInLimits)
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
    querier -> TokenHash -> Async (LoadSingleError errors) Int
ensureValidNonce querier nonceHash = do
    {playerId} :: {playerId :: Int} <-
        queryFirstNotFound querier nonceQueryString (nonceHash : [])
    pure playerId

-- The same link as `nonceQueryString` takes, left in place. Hashing the new
-- password waits its turn at bcrypt with every sign-in, so a link that can't
-- reset anything is turned away before it.
liveQueryString :: Query
liveQueryString = Query """
    select 1 as one
    from password_reset
    join player on player.id = password_reset.player_id
    where password_reset.nonce_hash = $1
        and password_reset.created > now() - interval '1 hour'
        and player.password_hash is not null
    """

ensureLiveNonce :: forall errors querier. Querier querier =>
    querier -> TokenHash -> Async (LoadSingleError errors) Unit
ensureLiveNonce querier nonceHash = do
    _ :: {one :: Int} <- queryFirstNotFound querier liveQueryString (nonceHash : [])
    pure unit

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

-- | The reset clears the account's failed sign-ins: whoever reset it holds
-- | its address, so it's theirs to sign in to.
resetPassword :: forall left.
    SignInLimits -> Bcrypt -> Pool -> ResetPassword.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow ResetPassword.BadContent + NotFoundRow_ + InternalRow_ + ()))
resetPassword signInLimits bcrypt pool {password, nonce} =
    sendResponse "Error resetting password" do
    -- Validate password.
    validPassword <- validatePassword' password

    nonceHash <- Token.hash $ Token nonce
    ensureLiveNonce pool nonceHash

    -- Generate password hash.
    hash <- generateHash bcrypt validPassword

    playerId <- pool # transaction \client -> do
        -- Ensure nonce is valid.
        playerId <- ensureValidNonce client nonceHash

        -- Update the password and end every session and other link.
        updatePassword client playerId hash
        revokeSessions client playerId
        deleteResets client playerId
        pure playerId

    fromEffect $ clear signInLimits.perAccount $ show playerId

    pure noContent_
