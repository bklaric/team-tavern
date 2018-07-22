module TeamTavern.Player.Domain.PlayerId where

import Prelude

import Data.Maybe (Maybe)
import TeamTavern.Domain.Id (Id)
import TeamTavern.Domain.Id as Id

newtype PlayerId = PlayerId Id

create :: Int -> Maybe PlayerId
create id = id # Id.create <#> PlayerId

derive newtype instance showPlayerId :: Show PlayerId
