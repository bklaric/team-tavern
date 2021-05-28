module TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), Input, gameHeader) where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (descriptionLeaderboard)
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeaderSection)
import TeamTavern.Client.Snippets.Class as HS

data ProfileTab = Players | Teams

derive instance eqProfileTab :: Eq ProfileTab

data Tab = Profiles ProfileTab | Competitions

derive instance eqTab :: Eq Tab

type Input = { handle :: String, title :: String, shortTitle :: String, tab :: Tab }

gameHeader :: forall slots action. Input -> HH.HTML slots action
gameHeader { handle, title, shortTitle, tab } = HH.div_
    [ contentHeader
        [ contentHeaderSection
            [ HH.h1 [ HS.class_ "content-heading" ]
                case tab of
                Profiles Players ->
                    [ HH.i [ HS.class_ "fas fa-user content-heading-icon" ] []
                    , HH.text $ title <> " players"
                    ]
                Profiles Teams ->
                    [ HH.i [ HS.class_ "fas fa-users content-heading-icon" ] []
                    , HH.text $ title <> " teams"
                    ]
                Competitions ->
                    [ HH.i [ HS.class_ "fas fa-trophy content-heading-icon" ] []
                    , HH.text $ title <> " competitions"
                    ]
            ]
        ]
    , contentDescription
        case tab of
        Profiles Players -> "Find " <> shortTitle <> " players looking for a team. Create your own player profile and let everyone know you're looking to team up."
        Profiles Teams -> "Find  " <> shortTitle <> " teams looking for players. Create your own team profile and recruit new members for your team."
        Competitions -> "Apply for open " <> shortTitle <> " leagues and tournaments and compete for prizes and boasting rights."
    , descriptionLeaderboard
    ]
