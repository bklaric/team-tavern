module TeamTavern.Profile.View.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Profile.View.Types (ViewError)

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing profile"
    viewError # match
        { invalidIdentifiers: \{ identifiers, errors } -> do
            logt $ "Couldn't validate identifiers: " <> show identifiers
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableViewModel: \{ foreignViewModel, errors } -> do
            logt $ "Couldn't read view model: "
                <> unsafeStringify foreignViewModel
            logt $ "Reading resulted in these errors: " <> show errors
        , notFound: \identifiers ->
            logt $ "Profile wasn't found: " <> show identifiers
        , invalidView: \viewModel ->
            logt $ "The view is invalid: " <> show viewModel
        }
