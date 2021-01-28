module TeamTavern.Routes.Shared.ExternalIdIlk where

import Prelude

import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ExternalIdIlk = Steam | Riot | Blizzard | PlayStation | XBox | Switch

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

type ExternalIdIlks = { head :: ExternalIdIlk, tail :: Array ExternalIdIlk }
