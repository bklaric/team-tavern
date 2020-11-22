module TeamTavern.Client.Pages.Home.FindProfiles where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionButtons, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionText)

findProfiles :: forall slots action. action -> HH.HTML slots action
findProfiles onClick =
    landingSection
    [ landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription "Search through players and teams who have already created their profiles for featured games on TeamTavern."
        , landingSectionButton "View all games" onClick
        ]
    , landingSectionImage "/images/search.png"
    ]

findProfiles' :: forall slots action. String -> action -> action -> HH.HTML slots action
findProfiles' title onPlayersClick onTeamsClick =
    landingSection
    [ landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription $ "Search through players and teams who have already created their " <> title <> " profiles on TeamTavern."
        , landingSectionButtons
            [ landingSectionButton "View player profiles" onPlayersClick
            , landingSectionButton "View team profiles" onTeamsClick
            ]
        ]
    , landingSectionImage "/images/search.png"
    ]
