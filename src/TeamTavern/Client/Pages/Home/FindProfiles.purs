module TeamTavern.Client.Pages.Home.FindProfiles where

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionText)

findProfiles onClick =
    landingSection
    [ landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription "Search through players and teams who have already created their profiles for featured games on TeamTavern."
        , landingSectionButton "View all games" onClick
        ]
    , landingSectionImage "/images/search.png"
    ]
