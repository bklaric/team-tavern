module TeamTavern.Server.Profile.Infrastructure.ConvertFields where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as Player
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team

convertFields :: Array Player.Field -> Array Team.Field
convertFields = mapMaybe
    case _ of
    { ilk, id, key, options: Just options } | ilk == 2 || ilk == 3 -> Just { id, key, options }
    _ -> Nothing
