module TeamTavern.Player.Register.Types.NoncedToken where

import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.Token (Token)

type NoncedToken = { token :: Token, nonce :: Nonce }
