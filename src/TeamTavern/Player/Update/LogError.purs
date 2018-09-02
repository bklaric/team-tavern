module TeamTavern.Player.Update.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.Update.Types (UpdateError)

logError :: UpdateError -> Effect Unit
logError updateError = do
    log "Error updating player"
    updateError # match
        { cantValidateTargetNickname: \errors ->
            logt $ "Validating target nickname resulted in these errors: "
                <> show errors
        , cookiesNotPresent: const $
            logt $ "Couldn't read requestor cookies"
        , nicknamesNotSame: const $
            logt $ "Target and requestor nicknames weren't same"
        , cantReadUpdateModel: \errors ->
            logt $ "Reading update model resulted in these errors: "
                <> show errors
        , cantValidateUpdate: \errors ->
            logt $ "Validating update resulted in these errors: "
                <> show errors
        , nicknameTaken: \{ nickname, error } ->
            logt $ "Nickname '" <> unwrap nickname <> "' is already taken: "
                <> print error
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        , notAuthorized: const $
            logt $ "Player update isn't authorized"
        }
