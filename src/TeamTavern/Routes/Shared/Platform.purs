module TeamTavern.Routes.Shared.Platform where

import Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString)
import Jarilo.FromComponent (class FromComponent)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

data Platform = Steam | Riot | BattleNet | PlayStation | Xbox | Switch

derive instance eqPlatform :: Eq Platform

derive instance genericPlatform :: Generic Platform _

instance showPlatform :: Show Platform where
    show = genericShow

fromString :: String -> Maybe Platform
fromString "steam" = Just Steam
fromString "riot" = Just Riot
fromString "battle.net" = Just BattleNet
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
toString PlayStation = "playstation"
toString Xbox = "xbox"
toString Switch = "switch"

instance readForeignPlatform :: ReadForeign Platform where
    readImpl platform' =
        readString platform'
        >>= (fromString' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignPlatform :: WriteForeign Platform where
    writeImpl platform = writeImpl $ toString platform

instance fromComponentPlatform :: FromComponent Platform where
    fromComponent platform = fromString' platform

type Platforms = { head :: Platform, tail :: Array Platform }
