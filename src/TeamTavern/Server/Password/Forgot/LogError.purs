module TeamTavern.Server.Password.Forgot.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Node.Errors as Node
import Postgres.Error as Postgres
import Postmark.Error as Postmark
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type ForgotError = Variant
    ( signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Cookies
        }
    , unreadableEmailAddress ::
        { content :: String
        , errors :: MultipleErrors
        }
    , randomError :: Node.Error
    , databaseError :: Postgres.Error
    , notFound :: String
    , unreadablePlayer ::
        { player :: Foreign
        , errors :: MultipleErrors
        }
    , sendEmailError :: Postmark.Error
    )

logError :: ForgotError -> Effect Unit
logError forgotError = do
    logStamped "Error sending reset password email"
    forgotError # match
        { signedIn: \{ cookieInfo, cookies } -> do
            logt $ "The request came with this player cookie info: "
                <> show cookieInfo
            logt $ "In these cookies: " <> show cookies
        , unreadableEmailAddress: \{ content, errors } -> do
            logt $ "Couldn't read email address from body: " <> content
            logt $ "Reading resulted in these errors: " <> show errors
        , randomError: \error ->
            logt $ "Generating random bytes resulted in this error: "
                <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notFound: \email ->
            logt $ "Couldn't find player with email: " <> email
        , unreadablePlayer: \{ player, errors } -> do
            logt $ "Couldn't read player: " <> unsafeStringify player
            logt $ "Reading resulted in these errors: " <> show errors
        , sendEmailError: \error -> do
            logt $ "Couldn't send email."
            logt $ "Email sending resulted in this error: "
                <> show error.statusCode <> ", " <> show error.code <> ", "
                <> error.message
        }
