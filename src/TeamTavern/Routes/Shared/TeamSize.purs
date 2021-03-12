module TeamTavern.Routes.Shared.TeamSize where

import Prelude

data TeamSize = Party | Community

derive instance eqTeamIlk :: Eq TeamSize
