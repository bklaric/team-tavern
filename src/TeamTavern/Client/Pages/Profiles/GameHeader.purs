module TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), Input, gameHeader) where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeaderSection, contentHeading')
import TeamTavern.Client.Snippets.Class as HS

data ProfileTab = Players | Teams

data Tab = Profiles ProfileTab

type Input = { title :: String, shortTitle :: String, tab :: Tab }

gameHeader :: ∀ slots action. Input -> HH.HTML slots action
gameHeader { title, shortTitle, tab } = HH.div_ $
    [ contentHeader
        [ contentHeaderSection
            [ contentHeading'
                case tab of
                Profiles Players -> [ HH.i [ HS.class_ "fas fa-user content-heading-icon" ] [], HH.text $ title <> " players / LFG / LFT" ]
                Profiles Teams -> [ HH.i [ HS.class_ "fas fa-users content-heading-icon" ] [], HH.text $ title <> " teams / LFM / LFP" ]
            ]
        ]
    , contentDescription
        case tab of
        Profiles Players -> "Find " <> shortTitle <> " players looking for a team. Create your own player profile and let everyone know you're looking to team up."
        Profiles Teams -> "Find  " <> shortTitle <> " teams looking for players. Create your own team profile and recruit new members for your team."
    ]
