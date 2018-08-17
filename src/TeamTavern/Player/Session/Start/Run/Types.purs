module TeamTavern.Player.Session.Start.Run.Types where

import Data.Variant (Variant)
import TeamTavern.Player.Infrastructure (ReadNicknameError)
import TeamTavern.Player.Session.Start.ConsumeToken (ConsumeTokenError)
import TeamTavern.Player.Session.Start.ReadNicknamedNonce (ReadNonceError)

type StartError = Variant
    ( readNickname :: ReadNicknameError
    , readNonce :: ReadNonceError
    , consumeToken :: ConsumeTokenError
    )
