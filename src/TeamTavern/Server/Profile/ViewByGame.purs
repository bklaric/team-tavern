module TeamTavern.Server.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Handle, ProfileIlk)
import TeamTavern.Server.Profile.ViewByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewByGame.SendResponse (sendResponse)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

viewByGame :: forall left. Pool -> Handle -> ProfileIlk -> QueryPairs Key Value -> Async left Response
viewByGame pool handle ilk filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle ilk filters

            -- Load profile count.
            count <- loadProfileCount client handle ilk filters

            pure { profiles, count }
