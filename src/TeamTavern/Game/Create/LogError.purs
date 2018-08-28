module TeamTavern.Game.Create.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Game.Create.Types (CreateError)
import TeamTavern.Player.Update.LogError (logt, print)

logError :: CreateError -> Effect Unit
logError createError = do
    log "Error creating game"
    createError # match
        { cookiesNotPresent: \cookies ->
            logt $ "Couldn't read player cookies: " <> show cookies
        , cantReadDetailsModel: \{ content, errors } -> do
            logt $ "Couldn't read details model from content: " <> content
            logt $ "Reading resulted in these errors: " <> show errors
        , cantValidateDetails: \{ name, description, errors } -> do
            logt $ "Couldn't validate details: " <> name <> ", " <> description
            logt $ "Validating resulted in these errors: " <> show errors
        , nameTaken: \{ name, error } ->
            logt $ "Name '" <> unwrap name <> "' is already taken: "
                <> print error
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        , notAuthorized: const $
            logt $ "Game creation isn't authorized"
        }
