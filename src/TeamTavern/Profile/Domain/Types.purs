module TeamTavern.Profile.Domain.Types where

import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname (Nickname)

type Identifiers =
    { nickname :: Nickname
    , handle :: Handle
    }
