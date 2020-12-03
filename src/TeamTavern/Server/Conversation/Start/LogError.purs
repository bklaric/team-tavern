module TeamTavern.Server.Conversation.Start.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import Postmark.Error as Postmark
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)

type StartError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , databaseError :: Error
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
    , unreadableResult ::
        { errors :: MultipleErrors
        , result :: Result
        }
    , notFound :: String
    , sendEmailError :: Postmark.Error
    )

logError :: StartError -> Effect Unit
logError startError = do
    logStamped "Error viewing conversation"
    startError # match
        { internal: logLines
        , client: logLines
        , notAuthenticated: logLines
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
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
        , unreadableResult: \{ result, errors } -> do
            logt $ "Couldn't read receiver info from result: " <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , notFound: \nickname ->
            logt $ "Couldn't find receiver info when loading from database for player: " <> nickname
        , sendEmailError: \error ->
            logt $ "Email sending error occured: " <> show error
        }
