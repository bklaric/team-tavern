module TeamTavern.Server.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewByGame.SendResponse (sendResponse)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

viewByGame :: forall left. Pool -> Handle -> QueryPairs Key Value -> Async left Response
viewByGame pool handle filters =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool handle filters
