module TeamTavern.Server.Player.UpdatePlayer.ReadPlayer where

import Async (Async)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import TeamTavern.Server.Infrastructure.Error (ClientError)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)

type PlayerModel =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , discordTag :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

readPlayer :: forall errors. Body -> Async (ClientError errors) PlayerModel
readPlayer = readJsonBody
