module TeamTavern.Client.Pages.Profiles.TeamBadge where

import Prelude

import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import TeamTavern.Client.Components.Checkbox (checkbox)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.TeamIlk (TeamIlk(..))

partyGroupBadge :: forall slots action. HH.HTML slots action
partyGroupBadge = HH.span [ HS.class_ "party-group-badge" ] [ HH.text "Party/Group" ]

communityOrganizationBadge :: forall slots action. HH.HTML slots action
communityOrganizationBadge =
    HH.span [ HS.class_ "community-organization-badge" ] [ HH.text "Community/Organization" ]

teamBadgeCheckboxes :: forall slots action.
    Array TeamIlk -> (TeamIlk -> action) -> HH.HTML slots action
teamBadgeCheckboxes selectedBadges onValue =
    HH.div [ HS.class_ "platform-id-checkboxes" ]
    [ HH.span
        [ HS.class_ "party-group-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue PartyGroup]
        [ HH.text "Party/Group"
        , checkbox $ elem PartyGroup selectedBadges
        ]
    , HH.span
        [ HS.class_ "community-organization-badge checkbox-container"
        , HP.onClick $ const $ Just $ onValue CommunityOrganization]
        [ HH.text "Community/Organization"
        , checkbox $ elem CommunityOrganization selectedBadges
        ]
    ]
