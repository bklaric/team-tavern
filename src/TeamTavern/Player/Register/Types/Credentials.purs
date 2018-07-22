module TeamTavern.Player.Register.Types.Credentials where

import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

type Credentials =
    { email :: Email
    , nickname :: Nickname
    , token :: Token
    , nonce :: Nonce
    }

type IdentifiedCredentials =
    { id :: PlayerId
    , email :: Email
    , nickname :: Nickname
    , token :: Token
    , nonce :: Nonce
    }
