module TeamTavern.Game.Update.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import TeamTavern.Game.Update.Types (UpdateError)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Infrastructure.Log (logt, print)

logError :: UpdateError -> Effect Unit
logError updateError = do
    logt "Error updating game"
    updateError # match
        { invalidTargetHandle: \{ handle, errors } -> do
            logt $ "Validating target game handle '" <> handle
                <> "' failed with these errors: " <> show errors
        , cookiesNotPresent: \cookies ->
            logt $ "Auth info cookies couldn't be read: " <> show cookies
        , unreadableDetailsModel: \{ content, errors } -> do
            logt $ "Couldn't read details out of content: " <> content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidDetails: \{ name, handle, description, errors } -> do
            logt $ "Validating game details failed: "
                <> name <> ", " <> handle <> ", " <> description
            logt $ "Validation resulted in these erorrs: " <> show errors
        , nameTaken: \{ name, error } ->
            logt $ "Game name '" <> unwrap name <> "' is already taken "
                <> "according to this error: " <> print error
        , handleTaken: \{ handle, error } ->
            logt $ "Game handle '" <> unwrap handle <> "' is already taken "
                <> "according to this error: " <> print error
        , databaseError: \error ->
            logt $ "An unknown database error occured: " <> print error
        , notAuthorized: \{ id, token, handle } ->
            logt $ "Player with id " <> toString id
                <> " and token " <> unwrap token
                <> " isn't authorized to update game with handle "
                <> unwrap handle
        }
