module TeamTavern.Game.View.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import TeamTavern.Game.View.Types (ViewError)
import TeamTavern.Infrastructure.Log (logt, print)

logError :: ViewError -> Effect Unit
logError viewError = do
    logt "Error viewing game"
    viewError # match
        { handleInvalid: \{ handle, errors } ->
            logt $ "Validating handle '" <> handle <> "' "
                <> "resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        , unreadableResult: \errors ->
            logt $ "Reading view resulted in these errors: " <> show errors
        , notFound: \handle ->
            logt $ "Game '" <> unwrap handle <> "' wasn't found"
        , invalidView: \{ administratorId, name, handle, description } ->
            logt $ "View of game '" <> unwrap handle <> "' is invalid: "
                <> show administratorId <> ", " <> name <> ", " <> description
        }
