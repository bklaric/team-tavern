module TeamTavern.Client.Pages.Home.FindProfiles where

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionText)
import Web.UIEvent.MouseEvent (MouseEvent)

findProfiles :: ∀ slots action. (MouseEvent -> action) -> HH.HTML slots action
findProfiles onClick =
    landingSection
    [ landingSectionImage Nothing "/images/search"
    , landingSectionText
        [ landingSectionHeading "fas fa-search" "Find your new teammates now!"
        , landingSectionDescription "Search through players and teams who have already created their profiles for featured games on TeamTavern."
        , landingSectionButton "View all games" "#games" onClick
        ]
    ]
