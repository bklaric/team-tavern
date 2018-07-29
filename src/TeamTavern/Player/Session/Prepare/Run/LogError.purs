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
        , readNickname: \{ errors, nickname } -> do
            log $ "\tFailed nickname validation for segment: "
                <> toString nickname
            log $ "\tValidation resulted in these errors: "
                <> show errors
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
        , createSession: \{ secrets: { nickname, token, nonce }, error } -> do
            log $ "\tError adding session to database for nickname "
                <> unwrap nickname <> " and secrets: "
                <> unwrap token <> ", " <> unwrap nonce
            error # match
                { unknownNickname: \result ->
                    log $ "\tNickname seems to be unknown"
                , invalidEmail: \errors ->
                    log $ "\tEmail validation resulted in these errors: "
                        <> show errors
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
