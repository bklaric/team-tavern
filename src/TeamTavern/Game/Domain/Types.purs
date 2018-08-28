module TeamTavern.Game.Domain.Types where

import TeamTavern.Game.Domain.Description (Description)
import TeamTavern.Game.Domain.Name (Name)

type Details =
    { name :: Name
    , description :: Description
    }
