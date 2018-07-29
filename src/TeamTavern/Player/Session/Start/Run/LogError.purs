module TeamTavern.Player.Session.Start.Run.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.Session.Start.Run.Types (StartError)

logStartError :: StartError -> Effect Unit
logStartError startError = do
    log "Error starting session"
    startError # match
        { ensureNotSignedIn: \{ playerId } ->
            log $ "\tThe request came with this player id: " <> playerId
        , readNickname: \{ errors, nickname } -> do
            log $ "\tFailed nickname validation for segment: "
                <> toString nickname
            log $ "\tValidation resulted in these errors: "
                <> show errors
        , readNonce: match
            { invalidBody: \{ errors, body } -> do
                log $ "\tCouldn't read nonce from body: " <> body
                log $ "\tParsing resulted in these errors: " <> show errors
            , invalidNonce: \{ errors, nonce } -> do
                log $ "\tFailed nonce validation for string: " <> nonce
                log $ "\tValidation resuldet in these errors: " <> show errors
            }
        , consumeToken: \{ nickname, nonce, error } -> error # match
            { noTokenToConsume: \result ->
                log $ "\tNo valid token to consume for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
            , cantReadIdentifiedToken: \result ->
                log $ "\tCouldn't read player id and token "
                    <> "from update response for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
            , other: \error' -> do
                log $ "\tCouldn't consume token for credentials: "
                    <> unwrap nickname <> ", " <> unwrap nonce
                log $ "\tConsuming resulted in this database error: "
                    <> code error' <> ", " <> name error' <> ", "
                    <> message error'
            }
        }
