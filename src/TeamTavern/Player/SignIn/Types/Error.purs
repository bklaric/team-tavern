module TeamTavern.Player.SignIn.Types.Error where

import Prelude

import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.SignIn.ConsumeToken (ConsumeTokenError)
import TeamTavern.Player.SignIn.ReadNicknamedNonce (ReadNicknameError, ReadNonceError)
import Unsafe.Coerce (unsafeCoerce)

type SignInError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readNickname :: ReadNicknameError
    , readNonce :: ReadNonceError
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
        , readNonce: match
            { invalidBody: \{ errors, body } -> do
                log $ "\tCouldn't read nonce from body: " <> body
                log $ "\tParsing resulted in these errors: " <> show errors
            , invalidNonce: \{ errors, nonce } ->
                log $ "\tCouldn't validate nonce from body: " <> nonce
            }
        , consumeToken: match
            { noTokenToConsume: \{ nickname, nonce } ->
                log $ "\tNo valid token to consume for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
            , cantReadIdentifiedToken: \{ result, nickname, nonce } ->
                log $ "\tCouldn't read player id and token "
                    <> "from update response for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
            , other: \{ error, nickname, nonce } -> do
                log $ "\tCouldn't consume token for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
                log $ "\tConsuming resulted in this database error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
            }
        }
