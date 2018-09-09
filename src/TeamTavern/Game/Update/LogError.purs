module TeamTavern.Game.Update.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import TeamTavern.Game.Update.Types (UpdateError)
import TeamTavern.Infrastructure.Log (logt, print)

logError :: UpdateError -> Effect Unit
logError updateError = do
    logt "Error updating game"
    updateError # match
        { invalidHandle: \{ handle, errors } -> do
            logt $ "Couldn't validate handle: " <> show handle
            logt $ "Validation resulted in these errors: " <> show errors
        , authNotPresent: \cookies ->
            logt $ "Couldn't read auth info out of cookies: " <> show cookies
        , unreadableDetails: \{ content, errors } -> do
            logt $ "Couldn't read details from content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidDetails: \{ details, errors } -> do
            logt $ "Couldn't validate details: " <> show details
            logt $ "Validating resulted in these errors: " <> show errors
        , nameTaken: \{ name, error } -> do
            logt $ "Name is already taken: " <> show name
            logt $ "According to this error: " <> print error
        , handleTaken: \{ handle, error } -> do
            logt $ "Handle is already taken: " <> show handle
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        , notAuthorized: \authInfo ->
            logt $ "Game creation isn't authorized for this auth info: "
                <> show authInfo
        }
