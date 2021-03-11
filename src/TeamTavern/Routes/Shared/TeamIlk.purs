module TeamTavern.Routes.Shared.TeamIlk where

import Prelude

data TeamIlk = PartyGroup | CommunityOrganization

derive instance eqTeamIlk :: Eq TeamIlk
