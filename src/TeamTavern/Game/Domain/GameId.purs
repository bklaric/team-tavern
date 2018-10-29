module TeamTavern.Game.Domain.GameId where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import TeamTavern.Domain.Id (Id)
import TeamTavern.Domain.Id as Id

newtype GameId = GameId Id

derive instance newtypeGameId :: Newtype GameId _

derive newtype instance showGameId :: Show GameId

create :: Int -> Maybe GameId
create id = id # Id.create <#> GameId

toInt :: GameId -> Int
toInt = unwrap >>> unwrap

toString :: GameId -> String
toString = toInt >>> show
