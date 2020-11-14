module TeamTavern.Server.Profile.Infrastructure.ConvertFields where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Record as Record
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as Player
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team

convertFields :: Array Player.Field -> Array Team.Field
convertFields = mapMaybe
    case _ of
    { id, key, options: Just options } | id == 2 || id == 3 ->
        Just { id, key, options: options <#> Record.delete (SProxy :: SProxy "domain") }
    _ -> Nothing
