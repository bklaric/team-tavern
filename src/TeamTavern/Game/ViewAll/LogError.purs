module TeamTavern.Game.ViewAll.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import TeamTavern.Game.ViewAll.Types (ViewAllError)
import TeamTavern.Player.Update.LogError (logt, print)

logError :: ViewAllError -> Effect Unit
logError viewAllError = do
    logt "Error viewing all games"
    viewAllError # match
        { invalidViews: \views ->
            logt $ "The following game views are invalid: " <> show views
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        , unreadableResult: \errors ->
            logt $ "Reading views resulted in these errors: " <> show errors
        }
