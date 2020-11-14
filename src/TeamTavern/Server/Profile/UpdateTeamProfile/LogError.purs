module TeamTavern.Server.Profile.UpdateTeamProfile.LogError where

import Prelude

import Data.Variant (Variant)
import Effect (Effect)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Profile.AddTeamProfile.LogError (profileHandler)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (ProfileErrors)

type UpdateProfileError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , profile :: ProfileErrors
    )

logError :: UpdateProfileError -> Effect Unit
logError = Log.logError "Error creating team profile"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> profileHandler
    )
