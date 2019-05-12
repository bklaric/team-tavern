module TeamTavern.Server.Player.Domain.Id where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Jarilo.FromComponent (class FromComponent)

newtype Id = Id Int

derive instance newtypeId :: Newtype Id _

derive instance genericId :: Generic Id _

instance showId :: Show Id where show = genericShow

derive newtype instance fromComponentPlayerId :: FromComponent Id

fromString :: String -> Maybe Id
fromString id = Int.fromString id <#> Id

toString :: Id -> String
toString = unwrap >>> show
