module TeamTavern.Routes.Shared.TeamOrganization where

import Prelude

data TeamOrganization = Informal | Organized

derive instance eqTeamOrganization :: Eq TeamOrganization
