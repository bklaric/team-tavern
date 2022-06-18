module TeamTavern.Routes.CreateAlert where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Foreign (ForeignError(..), fail)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Timezone (Timezone)

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
    Post
    (  Literal "alerts"
    :> End)
    NoQuery

type RequestContent =
    { handle :: String
    , playerOrTeam :: PlayerOrTeam
    , email :: String
    , timezone :: Timezone
    , filters :: Filters
    }

type BadContent = Variant (email :: Array String)
