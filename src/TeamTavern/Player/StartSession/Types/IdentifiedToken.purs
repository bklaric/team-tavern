module TeamTavern.Player.StartSession.Types.IdentifiedToken where

import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

type IdentifiedToken = { id :: PlayerId, token :: Token }
