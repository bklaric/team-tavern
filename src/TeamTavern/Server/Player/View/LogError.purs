module TeamTavern.Server.Player.View.LogError where

import Prelude

import Effect (Effect)
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Log (logLoadSingleError)

logError :: LoadSingleError () -> Effect Unit
logError = logLoadSingleError "Error viewing player"
