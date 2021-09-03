module TeamTavern.Server.Player.UpdatePlayer.LogError where

import Prelude

import Data.Variant (Variant)
import Effect (Effect)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log

type UpdateDetailsError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    )

logError :: UpdateDetailsError -> Effect Unit
logError = Log.logError "Error updating player"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    )
