module TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), Input, gameHeader) where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeaderSection, contentHeading')
import TeamTavern.Client.Snippets.Class as HS

data ProfileTab = Players | Teams

data Tab = Profiles ProfileTab

type Input = { title :: String, description :: Array String, tab :: Tab }

gameHeader :: ∀ slots action. Input -> Array (HH.HTML slots action)
gameHeader { title, description, tab } =
    [ contentHeader
        [ contentHeaderSection
            [ contentHeading'
                case tab of
                Profiles Players -> [ HH.i [ HS.class_ "fas fa-user content-heading-icon" ] [], HH.text $ title <> " players / LFG / LFT" ]
                Profiles Teams -> [ HH.i [ HS.class_ "fas fa-users content-heading-icon" ] [], HH.text $ title <> " teams / LFM / LFP" ]
            ]
        ]
    ]
    <> (description <#> contentDescription)
