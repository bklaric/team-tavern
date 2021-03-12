module TeamTavern.Routes.Shared.TeamSize where

import Prelude

import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

data TeamSize = Party | Community

derive instance eqTeamIlk :: Eq TeamSize

instance readForeignTeamSize :: ReadForeign TeamSize where
    readImpl platform' =
        readString platform' >>=
        case _ of
        "party" -> pure Party
        "community" -> pure Community
        platform -> fail $ ForeignError $ "Unknown organization: " <> platform

instance writeForeignTeamSize :: WriteForeign TeamSize where
    writeImpl Party = unsafeToForeign "party"
    writeImpl Community = unsafeToForeign "community"
