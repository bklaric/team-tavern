module TeamTavern.Server.Game.ViewAll.LogError where

import Prelude

import Effect (Effect)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (logInternalError)

type ViewAllError = InternalError ()

logError :: ViewAllError -> Effect Unit
logError = logInternalError "Error viewing all games"
