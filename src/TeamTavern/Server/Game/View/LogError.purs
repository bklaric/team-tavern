module TeamTavern.Server.Game.View.LogError where

import Prelude

import Effect (Effect)
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Log (logLoadSingleError)

type ViewError = LoadSingleError ()

logError :: ViewError -> Effect Unit
logError = logLoadSingleError "Error viewing game"
