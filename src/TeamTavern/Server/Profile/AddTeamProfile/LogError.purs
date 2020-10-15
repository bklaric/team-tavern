module TeamTavern.Server.Profile.AddTeamProfile.LogError where

import Prelude

import Data.Array as Array
import Data.Variant (SProxy(..), Variant, match)
import Effect (Effect, foreachE)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logLines, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (ProfileErrors)

type AddProfileError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthorized :: Array String
    , profile :: ProfileErrors
    )

profileHandler :: forall fields. Lacks "profile" fields =>
    Builder (Record fields) { profile :: ProfileErrors -> Effect Unit | fields }
profileHandler = Builder.insert (SProxy :: SProxy "profile") \errors ->
    foreachE (Array.fromFoldable errors) $ match { ambitions: logLines }

logError :: AddProfileError -> Effect Unit
logError = Log.logError "Error creating team profile"
    (internalHandler >>> clientHandler >>> notAuthorizedHandler >>> profileHandler)
