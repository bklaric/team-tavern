module TeamTavern.Server.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Maybe (Maybe)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Handle, ProfileIlk, ProfilePage, Age)
import TeamTavern.Server.Profile.ViewByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewByGame.SendResponse (sendResponse)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

viewByGame :: forall left. Pool -> Handle -> ProfileIlk -> ProfilePage -> Maybe Age -> Maybe Age -> QueryPairs Key Value -> Async left Response
viewByGame pool handle ilk page ageFrom ageTo filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle ilk page ageFrom ageTo filters

            -- Load profile count.
            count <- loadProfileCount client handle ilk ageFrom ageTo filters

            pure { profiles, count }
