module TeamTavern.Player.Domain.PlayerId where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import TeamTavern.Domain.Id (Id)
import TeamTavern.Domain.Id as Id

newtype PlayerId = PlayerId Id

derive instance newtypePlayerId :: Newtype PlayerId _

derive newtype instance showPlayerId :: Show PlayerId

create :: Int -> Maybe PlayerId
create id = id # Id.create <#> PlayerId

toString :: PlayerId -> String
toString = unwrap >>> unwrap >>> show
