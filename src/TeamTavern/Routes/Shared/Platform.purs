module TeamTavern.Routes.Shared.Platform where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Jarilo.FromComponent (class FromComponent)
import Simple.JSON (class ReadForeign, class WriteForeign)

data Platform = Steam | Riot | BattleNet | PlayStation | XBox | Switch

derive instance eqPlatform :: Eq Platform

derive instance genericPlatform :: Generic Platform _

instance showPlatform :: Show Platform where
    show = genericShow

instance readForeignPlatform :: ReadForeign Platform where
    readImpl ilk' =
        readString ilk' >>=
        case _ of
        "steam" -> pure Steam
        "riot" -> pure Riot
        "battle.net" -> pure BattleNet
        "playstation" -> pure PlayStation
        "xbox" -> pure XBox
        "switch" -> pure Switch
        ilk -> fail $ ForeignError $ "Unknown platform: " <> ilk

instance writeForeignPlatform :: WriteForeign Platform where
    writeImpl Steam = unsafeToForeign "steam"
    writeImpl Riot = unsafeToForeign "riot"
    writeImpl BattleNet = unsafeToForeign "battle.net"
    writeImpl PlayStation = unsafeToForeign "playstation"
    writeImpl XBox = unsafeToForeign "xbox"
    writeImpl Switch = unsafeToForeign "switch"

instance fromComponentPlatform :: FromComponent Platform where
    fromComponent "steam" = Right Steam
    fromComponent "riot" = Right Riot
    fromComponent "battle.net" = Right BattleNet
    fromComponent "playstation" = Right PlayStation
    fromComponent "xbox" = Right XBox
    fromComponent "switch" = Right Switch
    fromComponent platform = Left $ "Unknown platform: " <> platform

fromString :: String -> Maybe Platform
fromString "steam" = Just Steam
fromString "riot" = Just Riot
fromString "battle.net" = Just BattleNet
fromString "playstation" = Just PlayStation
fromString "xbox" = Just XBox
fromString "switch" = Just Switch
fromString _ = Nothing

toString :: Platform -> String
toString Steam = "steam"
toString Riot = "riot"
toString BattleNet = "battle.net"
toString PlayStation = "playstation"
toString XBox = "xbox"
toString Switch = "switch"

type Platforms = { head :: Platform, tail :: Array Platform }
