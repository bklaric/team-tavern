module TeamTavern.Server.Password.Reset.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Node.Errors as Node
import Postgres.Error as Postgres
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Password.Reset.ReadNewPassword (Nonce)
import TeamTavern.Server.Player.Domain.Password (PasswordError)

type ResetError = Variant
    ( internal :: Array String
    , signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Cookies
        }
    , unreadableNewPassword ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidPassword ::
        { password :: String
        , errors :: NonEmptyList PasswordError
        }
    , databaseError :: Postgres.Error
    , invalidNonce :: Nonce
    , unreadablePlayer ::
        { player :: Foreign
        , errors :: MultipleErrors
        }
    )

logError :: ResetError -> Effect Unit
logError forgotError = do
    logStamped "Error reseting password"
    forgotError # match
        { internal: logLines
        , signedIn: \{ cookieInfo, cookies } -> do
            logt $ "The request came with this player cookie info: "
                <> show cookieInfo
            logt $ "In these cookies: " <> show cookies
        , unreadableNewPassword: \{ content, errors } -> do
            logt $ "Couldn't read new password from body: " <> content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidPassword: \{ password, errors } -> do
            logt $ "Couldn't validate password: " <> password
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , invalidNonce: \nonce ->
            logt $ "Nonce is either invalid or has expired: " <> nonce
        , unreadablePlayer: \{ player, errors } -> do
            logt $ "Couldn't read player: " <> unsafeStringify player
            logt $ "Reading resulted in these errors: " <> show errors
        }
