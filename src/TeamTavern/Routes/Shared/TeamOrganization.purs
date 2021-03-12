module TeamTavern.Routes.Shared.TeamOrganization where

import Prelude

import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

data TeamOrganization = Informal | Organized

derive instance eqTeamOrganization :: Eq TeamOrganization

instance readForeignTeamOrganization :: ReadForeign TeamOrganization where
    readImpl platform' =
        readString platform' >>=
        case _ of
        "informal" -> pure Informal
        "organized" -> pure Organized
        platform -> fail $ ForeignError $ "Unknown organization: " <> platform

instance writeForeignTeamOrganization :: WriteForeign TeamOrganization where
    writeImpl Informal = unsafeToForeign "informal"
    writeImpl Organized = unsafeToForeign "organized"
