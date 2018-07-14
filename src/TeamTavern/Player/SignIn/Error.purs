module TeamTavern.Player.SignIn.Error where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (Variant, match)
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.SignIn.ConsumeToken (ConsumeTokenError)
import TeamTavern.Player.SignIn.ReadNickname (ReadNicknameError)
import TeamTavern.Player.SignIn.ReadToken (ReadTokenError)
import Unsafe.Coerce (unsafeCoerce)

type SignInError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readNickname :: ReadNicknameError
    , readToken :: ReadTokenError
    , consumeToken :: ConsumeTokenError
    )

logError :: SignInError -> Effect Unit
logError signInError = unsafeCoerce do
    log "Error signing in"
    signInError # match
        { ensureNotSignedIn: \{ token } ->
            log $ "\tThe request came with the following token: " <> token
        , readNickname: \{ errors, nickname } ->
            log $ "\tCouldn't read nickname from segment: " <> toString nickname
        , readToken: match
            { invalidBody: \{ errors, body } -> do
                log $ "\tCouldn't read token from body: " <> body
                log $ "\tParsing resulted in these errors: " <> show errors
            , invalidToken: \{ errors, token } ->
                log $ "\tCouldn't validate token from body: " <> token
            }
        , consumeToken: match
            { noTokenToConsume: \{ nickname, token } ->
                log $ "\tNo valid token to consume for credentials: "
                    <> unwrap nickname <> ", " <> unwrap token
            , other: \{ error, nickname, token } -> do
                log $ "\tCouldn't consume token for credentials: "
                    <> unwrap nickname <> ", " <> unwrap token
                log $ "\tConsuming resulted in this database error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
            }
        }
