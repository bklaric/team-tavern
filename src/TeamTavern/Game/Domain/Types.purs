module TeamTavern.Game.Domain.Types where

import TeamTavern.Game.Domain.Description (Description)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Player.Domain.PlayerId (PlayerId)

type Details =
    { title :: Title
    , handle :: Handle
    , description :: Description
    }

type View =
    { administratorId :: PlayerId
    , title :: Title
    , handle :: Handle
    , description :: Description
    }
