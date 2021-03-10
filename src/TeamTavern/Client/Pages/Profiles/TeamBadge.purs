module TeamTavern.Client.Pages.Profiles.TeamBadge where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

partyGroupBadge :: forall slots action. HH.HTML slots action
partyGroupBadge = HH.span [ HS.class_ "party-group-badge" ] [ HH.text "Party/Group" ]

communityOrganizationBadge :: forall slots action. HH.HTML slots action
communityOrganizationBadge =
    HH.span [ HS.class_ "community-organization-badge" ] [ HH.text "Community/Organization" ]
