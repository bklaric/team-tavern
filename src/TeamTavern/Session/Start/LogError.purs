module TeamTavern.Session.Start.LogError where

import Prelude

import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Node.Errors as Node
import Postgres.Error as Postgres
import Postgres.Result (Result, rows)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Session.Start.CheckPassword (PlayerId)
import TeamTavern.Session.Start.GenerateToken (Token)
import TeamTavern.Session.Start.ReadModel (NicknameOrEmail, Nonce)

type StartError = Variant
    ( signedIn ::
        { playerId :: String
        , cookies :: Map String String
        }
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , bcryptError :: Node.Error
    , randomError :: Node.Error
    , databaseError :: Postgres.Error
    , unreadableHash ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , noMatchingPlayer :: NicknameOrEmail
    , passwordDoesntMatch :: NicknameOrEmail
    , nothingConfirmed ::
        { playerId :: PlayerId
        , nonce :: Nonce
        }
    , unreadablePlayerId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , noSessionStarted ::
        { playerId :: PlayerId
        , token :: Token
        }
    )

logError :: StartError -> Effect Unit
logError startError = do
    log "Error starting session"
    startError # match
        { signedIn: \{ playerId, cookies } -> do
            logt $ "The request came with this player id: " <> show playerId
            logt $ "In these cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , bcryptError: \error ->
            logt $ "Password comparing resulted in this error: " <> print error
        , randomError: \error ->
            logt $ "Generating random bytes resulted in this error: "
                <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableHash: \{ result, errors } -> do
            logt $ "Can't read password hash from from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , noMatchingPlayer: \nicknameOrEmail ->
            logt $ "No matching player found for nickname or email: "
                <> show nicknameOrEmail
        , passwordDoesntMatch: \nicknameOrEmail ->
            logt $ "Entered password doesn't match for player: "
                <> show nicknameOrEmail
        , nothingConfirmed: \info ->
            logt $ "No email confirmed: " <> show info
        , unreadablePlayerId: \{ result, errors } -> do
            logt $ "Couldn't read player id from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , noSessionStarted: \info ->
            logt $ "No session started: " <> show info
        }
