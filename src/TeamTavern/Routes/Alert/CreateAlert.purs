module TeamTavern.Routes.Alert.CreateAlert where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Foreign (ForeignError(..), fail)
import Jarilo.Method (Post)
import Jarilo.Path (Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Types (Timezone)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data PlayerOrTeam = Player | Team

fromString :: String -> Maybe PlayerOrTeam
fromString "player" = Just Player
fromString "team" = Just Team
fromString _ = Nothing

toString :: PlayerOrTeam -> String
toString Player = "player"
toString Team = "team"

instance writeForeignPlayerOrTeam :: WriteForeign PlayerOrTeam where
    writeImpl Player = writeImpl "player"
    writeImpl Team = writeImpl "team"

instance readForeignPlayerOrTeam :: ReadForeign PlayerOrTeam where
    readImpl = readImpl >=> case _ of
        "player" -> pure Player
        "team" -> pure Team
        string -> fail $ ForeignError $ "Invalid player or team " <> string

type CreateAlert = FullRoute
    (Post RequestContent)
    (  Literal "alerts")
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent =
    { handle :: String
    , playerOrTeam :: PlayerOrTeam
    , email :: String
    , timezone :: Timezone
    , filters :: Filters
    }

type BadContent = Variant (email :: Array String)
