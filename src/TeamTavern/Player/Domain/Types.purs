module TeamTavern.Player.Domain.Types where

import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

type Secrets =
    { token :: Token
    , nonce :: Nonce
    }

type Identifiers =
    { email :: Email
    , nickname :: Nickname
    }

type IdentifiedToken =
    { id :: PlayerId
    , token :: Token
    }

type IdentifiedToken' =
    { id :: PlayerId
    , nickname :: Nickname
    , token :: Token
    }

type NicknamedNonce =
    { nickname :: Nickname
    , nonce :: Nonce
    }

type NicknamedSecrets =
    { nickname :: Nickname
    , token :: Token
    , nonce :: Nonce
    }

type NoncedIdentifiers =
    { nickname :: Nickname
    , email :: Email
    , nonce :: Nonce
    }

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
