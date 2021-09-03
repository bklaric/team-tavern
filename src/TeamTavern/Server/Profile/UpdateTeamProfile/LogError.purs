module TeamTavern.Server.Profile.UpdateTeamProfile.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Effect (Effect)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Profile.AddTeamProfile.LogError (invalidBodyHandler)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (ProfileErrors)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (ContactsErrors)
import Type (type ($))

type UpdateProfileError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , invalidBody :: NonEmptyList $ Variant
        ( teamProfile :: ProfileErrors
        , teamContacts :: ContactsErrors
        )
    )

logError :: UpdateProfileError -> Effect Unit
logError = Log.logError "Error updating team profile"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> invalidBodyHandler
    )
