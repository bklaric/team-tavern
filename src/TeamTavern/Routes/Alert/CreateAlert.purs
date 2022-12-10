module TeamTavern.Routes.Alert.CreateAlert where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Foreign (ForeignError(..), fail)
import Jarilo (type (!), type (==>), BadRequestJson, Literal, NoContent, PostJson_, Internal_)
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

instance WriteForeign PlayerOrTeam where
    writeImpl Player = writeImpl "player"
    writeImpl Team = writeImpl "team"

instance ReadForeign PlayerOrTeam where
    readImpl = readImpl >=> case _ of
        "player" -> pure Player
        "team" -> pure Team
        string -> fail $ ForeignError $ "Invalid player or team " <> string

type CreateAlert =
    PostJson_ (Literal "alerts") RequestContent
    ==> NoContent ! BadRequestJson BadContent ! Internal_

type RequestContent =
    { handle :: String
    , playerOrTeam :: PlayerOrTeam
    , email :: String
    , timezone :: Timezone
    , filters :: Filters
    }

type BadContent = Variant (email :: {})
