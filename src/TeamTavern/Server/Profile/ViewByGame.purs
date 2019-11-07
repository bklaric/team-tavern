module TeamTavern.Server.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Handle, ProfileIlk)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewByGame.SendResponse (sendResponse)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

viewByGame :: forall left. Pool -> Handle -> ProfileIlk -> QueryPairs Key Value -> Async left Response
viewByGame pool handle ilk filters =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool handle ilk filters
