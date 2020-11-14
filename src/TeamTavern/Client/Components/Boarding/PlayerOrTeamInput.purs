module TeamTavern.Client.Components.Boarding.PlayerOrTeamInput where

import Prelude

import Data.Maybe (Maybe, maybe)
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Simple.JSON (class ReadForeign, class WriteForeign)
import TeamTavern.Client.Components.RadioButton (radioButton, radioButtons)
import TeamTavern.Client.Snippets.Class as HS

data PlayerOrTeam = Player | Team

instance writeForeignPlayerOrTeam :: WriteForeign PlayerOrTeam where
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"

instance readForeignPlayerOrTeam :: ReadForeign PlayerOrTeam where
    readImpl = readString >=> case _ of
        "Player" -> pure Player
        "Team" -> pure Team
        string -> fail $ ForeignError $ "Invalid player or team " <> string

isPlayer :: PlayerOrTeam -> Boolean
isPlayer Player = true
isPlayer Team = false

isTeam :: PlayerOrTeam -> Boolean
isTeam Player = false
isTeam Team = true

playerOrTeamInput :: forall slots action.
    Maybe PlayerOrTeam -> (PlayerOrTeam -> action) -> HTML slots action
playerOrTeamInput selected onSelect =
    radioButtons
    [ radioButton (maybe false isPlayer selected) (onSelect Player)
        [ HH.i [ HS.class_ "fas fa-user button-icon" ] []
        , HH.text "Create player profile"
        ]
    , radioButton (maybe false isTeam selected) (onSelect Team)
        [ HH.i [ HS.class_ "fas fa-users button-icon" ] []
        , HH.text "Create team profile"
        ]
    ]
