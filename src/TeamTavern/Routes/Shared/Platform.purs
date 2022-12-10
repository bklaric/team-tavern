module TeamTavern.Routes.Shared.Platform where

import Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString)
import Jarilo.Shared.Component (class Component)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

data Platform = Steam | Riot | BattleNet | Origin | Ubisoft | PlayStation | Xbox | Switch

derive instance Eq Platform

derive instance Ord Platform

derive instance Generic Platform _

instance Show Platform where
    show = genericShow

fromString :: String -> Maybe Platform
fromString "steam" = Just Steam
fromString "riot" = Just Riot
fromString "battle.net" = Just BattleNet
fromString "origin" = Just Origin
fromString "ubisoft-connect" = Just Ubisoft
fromString "playstation" = Just PlayStation
fromString "xbox" = Just Xbox
fromString "switch" = Just Switch
fromString _ = Nothing

fromString' :: String -> Either String Platform
fromString' platform = fromString platform # note ("Unknown platform: " <> platform)

toString :: Platform -> String
toString Steam = "steam"
toString Riot = "riot"
toString BattleNet = "battle.net"
toString Origin = "origin"
toString Ubisoft = "ubisoft-connect"
toString PlayStation = "playstation"
toString Xbox = "xbox"
toString Switch = "switch"

toLabel :: Platform -> String
toLabel Steam = "Steam"
toLabel Riot = "Riot"
toLabel BattleNet = "Battle.net"
toLabel Origin = "Origin"
toLabel Ubisoft = "Ubisoft Connect"
toLabel PlayStation = "PlayStation"
toLabel Xbox = "Xbox"
toLabel Switch = "Switch"

instance ReadForeign Platform where
    readImpl platform' =
        readString platform'
        >>= (fromString' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance WriteForeign Platform where
    writeImpl platform = writeImpl $ toString platform

instance Component Platform where
    fromComponent = fromString'
    toComponent = toString

type Platforms = { head :: Platform, tail :: Array Platform }
