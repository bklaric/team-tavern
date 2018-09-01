module TeamTavern.Game.Domain.Types where

import TeamTavern.Game.Domain.Description (Description)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Name (Name)
import TeamTavern.Player.Domain.PlayerId (PlayerId)

type Details =
    { name :: Name
    , handle :: Handle
    , description :: Description
    }

type View =
    { administratorId :: PlayerId
    , name :: Name
    , handle :: Handle
    , description :: Description
    }
