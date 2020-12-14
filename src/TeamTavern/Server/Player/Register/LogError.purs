module TeamTavern.Server.Player.Register.LogError where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, match)
import Effect (Effect, foreachE)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Node.Errors as Node
import Postgres.Error as Postgres
import Postgres.Result (Result, rows)
import Postmark.Error as Postmark
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (internalHandler, logLines, logStamped, logt, print)
import TeamTavern.Server.Player.Domain.Email (Email)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Register.ValidateRegistration (RegistrationErrors)

type RegisterError = Variant
    ( internal :: Array String
    , signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Map String String
        }
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , registration :: RegistrationErrors
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
    , cantReadId :: Result
    , sendEmailError ::
        { info :: { email :: Email, nickname :: Nickname }
        , error :: Postmark.Error
        }
    )

registrationHandler :: forall fields. Lacks "registration" fields =>
    Builder (Record fields) { registration :: RegistrationErrors -> Effect Unit | fields }
registrationHandler = Builder.insert (SProxy :: SProxy "registration") \errors ->
    foreachE (Array.fromFoldable errors) $ match
        { nickname: logLines
        , email: logLines
        , password: logLines
        }

logError :: RegisterError -> Effect Unit
logError registerError = do
    logStamped "Error registering player"
    registerError # match
        (Builder.build (internalHandler >>> registrationHandler)
        { signedIn: \{ cookieInfo, cookies } -> do
            logt $ "The request came with this player cookie info: "
                <> show cookieInfo
            logt $ "In these cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , randomError: \error ->
            logt $ "Generating random bytes resulted in this error: "
                <> print error
        , emailTaken: \{ email, error } -> do
            logt $ "Email is already taken: " <> unwrap email
            logt $ "According to this error: " <> print error
        , nicknameTaken: \{ nickname, error } -> do
            logt $ "Nickname is already taken: " <> unwrap nickname
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , cantReadId: \result ->
            logt $ "Can't read player id from response: "
                <> (unsafeStringify $ rows result)
        , sendEmailError: \{ info: { email }, error } -> do
            logt $ "Couldn't send email to address: " <> unwrap email
            logt $ "Email sending resulted in this error: "
                <> show error.statusCode <> ", " <> show error.code <> ", "
                <> error.message
        }
        )
