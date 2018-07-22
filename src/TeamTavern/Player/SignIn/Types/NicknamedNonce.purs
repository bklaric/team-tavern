module TeamTavern.Player.SignIn.Types.NicknamedNonce where

import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)

type NicknamedNonce = { nickname :: Nickname, nonce :: Nonce }
