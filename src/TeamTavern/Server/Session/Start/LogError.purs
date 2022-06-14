module TeamTavern.Server.Session.Start.LogError where

import Prelude

import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Yoga.JSON (unsafeStringify)
import Node.Errors as Node
import Postgres.Error as Postgres
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Session.Domain.Token (Token)

type StartError = Variant
    ( internal :: Array String
    , signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Map String String
        }
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , bcrypt :: Node.Error
    , randomError :: Node.Error
    , databaseError :: Postgres.Error
    , unreadableHash ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , noMatchingPlayer :: String
    , passwordDoesntMatch :: String
    , noSessionStarted ::
        { id :: Id
        , token :: Token
        }
    )

logError :: StartError -> Effect Unit
logError startError = do
    logStamped "Error starting session"
    startError # match
        { internal: logLines
        , signedIn: \{ cookieInfo, cookies } -> do
            logt $ "The request came with this player info: " <> show cookieInfo
            logt $ "In these cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , bcrypt: \error ->
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
            logt $ "No matching player found for nickname: "
                <> show nicknameOrEmail
        , passwordDoesntMatch: \nicknameOrEmail ->
            logt $ "Entered password doesn't match for player: "
                <> show nicknameOrEmail
        , noSessionStarted: \info ->
            logt $ "No session started: " <> show info
        }
