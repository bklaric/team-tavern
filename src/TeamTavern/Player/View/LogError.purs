module TeamTavern.Player.View.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Player.Update.LogError (logt, print)
import TeamTavern.Player.View.Types (ViewError)

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing player"
    viewError # match
        { nicknameInvalid: \{ nickname, errors } ->
            logt $ "Validating nickname '" <> toString nickname <> "' "
                <> "resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        , unreadableResult: \errors ->
            logt $ "Reading view resulted in these errors: " <> show errors
        , notFound: \nickname ->
            logt $ "Player '" <> unwrap nickname <> "' wasn't found"
        , invalidView: \{ nickname, about } ->
            logt $ "View of player '" <> unwrap nickname <> "' is invalid: "
                <> about
        }
