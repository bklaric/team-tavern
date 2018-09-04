module TeamTavern.Profile.Domain.Types where

import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.Domain.Summary (Summary)

type Identifiers =
    { nickname :: Nickname
    , handle :: Handle
    }

type ByGameView =
    { nickname :: Nickname
    , summary :: Summary
    }
