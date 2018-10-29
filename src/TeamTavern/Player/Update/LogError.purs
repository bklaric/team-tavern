module TeamTavern.Player.Update.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.Update.Types (UpdateError)

logError :: UpdateError -> Effect Unit
logError updateError = do
    log "Error updating player"
    updateError # match
        { invalidNickname: \{ nickname, errors } -> do
            logt $ "Couldn't validate nickname: " <> show nickname
            logt $ "Validation resulted in these errors: " <> show errors
        , authNotPresent: \cookies ->
            logt $ "Couldn't read auth info out of cookies: " <> show cookies
        , unreadableUpdate: \{ content, errors } -> do
            logt $ "Couldn't read update from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidUpdate: \{ nicknamedAbout, errors } -> do
            logt $ "Couldn't validate update: " <> show nicknamedAbout
            logt $ "Validation resulted in these errors: " <> show errors
        , nicknameTaken: \{ nickname, error } -> do
            logt $ "Nickname is already taken: " <> show nickname
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notAuthorized: \{ authInfo, nickname } -> do
            logt $ "Player with auth: " <> show authInfo
            logt $ "Not authorized to update player: " <> show nickname
        }
