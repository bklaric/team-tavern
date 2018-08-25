module TeamTavern.Player.View.Run.Types where

import Data.Variant (Variant)
import TeamTavern.Player.Infrastructure (ReadNicknameError)
import TeamTavern.Player.View.LoadPlayer (LoadPlayerError)

type ViewError = Variant
    ( readNickname :: ReadNicknameError
    , loadPlayer :: LoadPlayerError
    )
