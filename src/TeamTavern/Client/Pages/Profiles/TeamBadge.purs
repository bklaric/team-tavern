module TeamTavern.Client.Pages.Profiles.TeamBadge where

import Prelude

import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import TeamTavern.Client.Components.Checkbox (checkbox)
import TeamTavern.Client.Components.Radio (radio)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.TeamOrganization (TeamOrganization(..))
import TeamTavern.Routes.Shared.TeamSize (TeamSize(..))

partyBadge :: forall slots action. HH.HTML slots action
partyBadge = HH.span [ HS.class_ "party-badge" ] [ HH.text "Party" ]

communityBadge :: forall slots action. HH.HTML slots action
communityBadge = HH.span [ HS.class_ "community-badge" ] [ HH.text "Community" ]

informalBadge :: forall slots action. HH.HTML slots action
informalBadge = HH.span [ HS.class_ "informal-badge" ] [ HH.text "Informal" ]

organizedBadge :: forall slots action. HH.HTML slots action
organizedBadge = HH.span [ HS.class_ "organized-badge" ] [ HH.text "Organized" ]

partyGroupBadge :: forall slots action. HH.HTML slots action
partyGroupBadge = HH.span [ HS.class_ "party-group-badge" ] [ HH.text "Party/Group" ]

communityOrganizationBadge :: forall slots action. HH.HTML slots action
communityOrganizationBadge =
    HH.span [ HS.class_ "community-organization-badge" ] [ HH.text "Community/Organization" ]

teamOrganizationCheckboxes :: forall slots action.
    Array TeamOrganization -> (TeamOrganization -> action) -> HH.HTML slots action
teamOrganizationCheckboxes selected onValue =
    HH.div [ HS.class_ "platform-id-checkboxes" ]
    [ HH.span
        [ HS.class_ "informal-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue Informal]
        [ HH.text "Informal"
        , checkbox $ elem Informal selected
        ]
    , HH.span
        [ HS.class_ "organized-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue Organized]
        [ HH.text "Organized"
        , checkbox $ elem Organized selected
        ]
    ]

teamSizeCheckboxes :: forall slots action.
    Array TeamSize -> (TeamSize -> action) -> HH.HTML slots action
teamSizeCheckboxes selected onValue =
    HH.div [ HS.class_ "platform-id-checkboxes" ]
    [ HH.span
        [ HS.class_ "party-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue Party]
        [ HH.text "Party"
        , checkbox $ elem Party selected
        ]
    , HH.span
        [ HS.class_ "community-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue Community]
        [ HH.text "Community"
        , checkbox $ elem Community selected
        ]
    ]

teamOrganizationRadios :: forall slots action.
    TeamOrganization -> (TeamOrganization -> action) -> HH.HTML slots action
teamOrganizationRadios selected onValue =
    HH.div [ HS.class_ "platform-id-checkboxes" ]
    [ HH.span
        [ HS.class_ "informal-badge radio-container"
        , HP.onClick $ const $ Just $ onValue Informal]
        [ HH.text "Informal"
        , radio $ Informal == selected
        ]
    , HH.span
        [ HS.class_ "organized-badge radio-container"
        , HP.onClick $ const $ Just $ onValue Organized]
        [ HH.text "Organized"
        , radio $ Organized == selected
        ]
    ]

teamSizeRadios :: forall slots action.
    TeamSize -> (TeamSize -> action) -> HH.HTML slots action
teamSizeRadios selected onValue =
    HH.div [ HS.class_ "platform-id-checkboxes" ]
    [ HH.span
        [ HS.class_ "party-badge radio-container"
        , HP.onClick $ const $ Just $ onValue Party]
        [ HH.text "Party"
        , radio $ Party == selected
        ]
    , HH.span
        [ HS.class_ "community-badge radio-container"
        , HP.onClick $ const $ Just $ onValue Community]
        [ HH.text "Community"
        , radio $ Community == selected
        ]
    ]
