module TeamTavern.Client.Pages.Home.FindProfiles where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionButtons, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionText)
import Web.UIEvent.MouseEvent (MouseEvent)

findProfiles :: forall slots action. (MouseEvent -> action) -> HH.HTML slots action
findProfiles onClick =
    landingSection
    [ landingSectionImage "/images/search.png"
    , landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription "Search through players and teams who have already created their profiles for featured games on TeamTavern."
        , landingSectionButton "View all games" "/games" onClick
        ]
    ]

findProfiles' :: forall slots action.
    String -> String -> (MouseEvent -> action) -> (MouseEvent -> action) -> HH.HTML slots action
findProfiles' handle title onPlayersClick onTeamsClick =
    landingSection
    [ landingSectionImage $ "/images/" <> handle <> "/search.png"
    , landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription $ "Search through players and teams who have already created their " <> title <> " profiles on TeamTavern."
        , landingSectionButtons
            [ landingSectionButton "View player profiles" ("/games/" <> handle <> "/players") onPlayersClick
            , landingSectionButton "View team profiles" ("/games/" <> handle <> "/teams") onTeamsClick
            ]
        ]
    ]
