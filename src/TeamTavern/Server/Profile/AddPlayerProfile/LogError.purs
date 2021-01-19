module TeamTavern.Server.Profile.AddPlayerProfile.LogError where

import Prelude

import Data.Array as Array
import Data.Variant (SProxy(..), Variant, match)
import Effect (Effect, foreachE)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logLines, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (ProfileErrors)

type CreateError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , profile :: ProfileErrors
    )

profileHandler :: forall fields. Lacks "profile" fields =>
    Builder (Record fields) { profile :: ProfileErrors -> Effect Unit | fields }
profileHandler = Builder.insert (SProxy :: SProxy "profile") \errors ->
    foreachE (Array.fromFoldable errors) $ match
        { url: _.message >>> logLines
        , ambitions: logLines
        }

logError :: CreateError -> Effect Unit
logError = Log.logError "Error creating team profile"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> profileHandler
    )
