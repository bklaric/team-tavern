module TeamTavern.Player.Session.Prepare.Run.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.Session.Prepare.Run.Types (PrepareError)

logError :: PrepareError -> Effect Unit
logError prepareError = do
    log "Error preparing session"
    prepareError # match
        { ensureNotSignedIn: \{ playerId } ->
            log $ "\tThe request came with this player id: " <> playerId
        , readIdentifiers: \{ errors, nickname, body } -> do
            log $ "\tCouldn't read identifiers from nickname and body: "
                <> toString nickname <> ", " <> show body
            log $ "\tParsing resulted in these errors: " <> show errors
        , validateIdentifiers: \{ errors, model } ->
            log $ "\tFailed identifiers validation for identifiers: "
                <> model.email <> ", " <> model.nickname
        , generateSecrets: \{ error, nickname } -> do
            log $ "\tError generating token for nickname: " <> unwrap nickname
            error # match
                { random: \error' ->
                    log $ "\tGenerating random bytes resulted in this error: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                , token: \errors ->
                    log $ "\tToken validation resulted in these errors: "
                        <> show errors
                , nonce: \errors ->
                    log $ "\tNonce validation resulted in these errors: "
                        <> show errors
                }
        , createSession:
            \{ credentials: { email, nickname, token, nonce }, error } -> do
            log $ "\tError adding session to database for credentials "
                <> unwrap email <> ", " <> unwrap nickname <> ", "
                <> unwrap token <> ", " <> unwrap nonce
            error # match
                { unknownIdentifiers: \result ->
                    log $ "\tIdentifiers seem to be unknown"
                , other: \error' ->
                    log $ "\tAdding to database resulted in this error: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                }
        , notifyPlayer: \{ identifiers: { nickname, email }, error } -> do
            log $ "\tError sending email to player with identifiers "
                <> unwrap nickname <> ", " <> unwrap email
            log $ "\tEmail sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }
