module TeamTavern.Server.Player.View.LogError where

import Prelude

import Effect (Effect)
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Log (logError)

logError :: LoadSingleError () -> Effect Unit
logError = logError "Error viewing player"
