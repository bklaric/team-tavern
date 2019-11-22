module TeamTavern.Server.Conversation.Start.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type StartError = Variant
    ( noCookieInfo ::
        { cookies :: Cookies
        }
    , databaseError :: Error
    , invalidSession ::
        { cookieInfo :: CookieInfo
        }
    , unreadableConversationId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , nothingInsertedSomehow ::
        { result :: Result
        }
    , unreadableMessage ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidMessage ::
        { message :: String
        , errors :: NonEmptyList NonEmptyTextError
        }
    )

logError :: StartError -> Effect Unit
logError startError = do
    logStamped "Error viewing conversation"
    startError # match
        { noCookieInfo: \{ cookies } ->
            logt $ "No player info present in cookies: " <> show cookies
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        , invalidSession: \{ cookieInfo } ->
            logt $ "Player has invalid session info in cookies: " <> show cookieInfo
        , unreadableConversationId: \{ result, errors } -> do
            logt $ "Couldn't read conversation id from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading conversation id resulted in these errors: " <> show errors
        , nothingInsertedSomehow: \{ result } ->
            logt $ "Nothing was inserted while creating conversation: " <> (unsafeStringify $ rows result)
        , unreadableMessage: \{ content, errors } -> do
            logt $ "Couldn't read message from body: " <> content
            logt $ "Reading message resulted in these errors: " <> show errors
        , invalidMessage: \{ message, errors } -> do
            logt $ "Couldn't validate message: " <> message
            logt $ "Validating message resulted in these errors: " <> show errors
        }
