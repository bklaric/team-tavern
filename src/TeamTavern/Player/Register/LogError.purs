module TeamTavern.Player.Register.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Node.Errors as Node
import Postgres.Error as Postgres
import Postmark.Error as Postmark
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.Register.ReadDto (RegisterDto)
import TeamTavern.Player.Register.ValidateModel (Email, Nickname, RegisterModelError, unEmail, unNickname)

type RegisterError = Variant
    ( signedIn ::
        { playerId :: String
        , cookies :: Map String String
        }
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: RegisterDto
        , errors :: NonEmptyList RegisterModelError
        }
    , bcryptError :: Node.Error
    , randomError :: Node.Error
    , emailTaken ::
        { email :: Email
        , error :: Postgres.Error
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Postgres.Error
        }
    , databaseError :: Postgres.Error
    , sendEmailError ::
        { info :: { email :: Email, nickname :: Nickname }
        , error :: Postmark.Error
        }
    )

logError :: RegisterError -> Effect Unit
logError registerError = do
    log "Error registering player"
    registerError # match
        { signedIn: \{ playerId, cookies } -> do
            logt $ "The request came with this player id: " <> show playerId
            logt $ "In these cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidModel: \{ dto, errors } -> do
            logt $ "Couldn't validate model: " <> show dto
            logt $ "Validation resulted in these errors: " <> show errors
        , bcryptError: \error ->
            logt $ "Password hashing resulted in this error: " <> print error
        , randomError: \error ->
            logt $ "Generating random bytes resulted in this error: "
                <> print error
        , emailTaken: \{ email, error } -> do
            logt $ "Email is already taken: " <> unEmail email
            logt $ "According to this error: " <> print error
        , nicknameTaken: \{ nickname, error } -> do
            logt $ "Nickname is already taken: " <> unNickname nickname
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , sendEmailError: \{ info: { email }, error } -> do
            logt $ "Couldn't send email to address: " <> unEmail email
            logt $ "Email sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }
