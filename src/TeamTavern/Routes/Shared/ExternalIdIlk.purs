module TeamTavern.Routes.Shared.ExternalIdIlk where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Jarilo.FromComponent (class FromComponent)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ExternalIdIlk = Steam | Riot | Blizzard | PlayStation | XBox | Switch

derive instance eqExternalIdIlk :: Eq ExternalIdIlk

derive instance genericExternalIdIlk :: Generic ExternalIdIlk _

instance showExternalIdIlk :: Show ExternalIdIlk where
    show = genericShow

instance readForeignExternalIdIlk :: ReadForeign ExternalIdIlk where
    readImpl ilk' =
        readString ilk' >>=
        case _ of
        "steam" -> pure Steam
        "riot" -> pure Riot
        "blizzard" -> pure Blizzard
        "playstation" -> pure PlayStation
        "xbox" -> pure XBox
        "switch" -> pure Switch
        ilk -> fail $ ForeignError $ "Unknown external id ilk: " <> ilk

instance writeForeignExternalIdIlk :: WriteForeign ExternalIdIlk where
    writeImpl Steam = unsafeToForeign "steam"
    writeImpl Riot = unsafeToForeign "riot"
    writeImpl Blizzard = unsafeToForeign "blizzard"
    writeImpl PlayStation = unsafeToForeign "playstation"
    writeImpl XBox = unsafeToForeign "xbox"
    writeImpl Switch = unsafeToForeign "switch"

instance fromComponentExternalIdIlk :: FromComponent ExternalIdIlk where
    fromComponent "steam" = Right Steam
    fromComponent "riot" = Right Riot
    fromComponent "blizzard" = Right Blizzard
    fromComponent "playstation" = Right PlayStation
    fromComponent "xbox" = Right XBox
    fromComponent "switch" = Right Switch
    fromComponent externalIdIlk = Left $ "Unknown external id ilk: " <> externalIdIlk

fromString :: String -> Maybe ExternalIdIlk
fromString "steam" = Just Steam
fromString "riot" = Just Riot
fromString "blizzard" = Just Blizzard
fromString "playstation" = Just PlayStation
fromString "xbox" = Just XBox
fromString "switch" = Just Switch
fromString _ = Nothing

toString :: ExternalIdIlk -> String
toString Steam = "steam"
toString Riot = "riot"
toString Blizzard = "blizzard"
toString PlayStation = "playstation"
toString XBox = "xbox"
toString Switch = "switch"

type ExternalIdIlks = { head :: ExternalIdIlk, tail :: Array ExternalIdIlk }
