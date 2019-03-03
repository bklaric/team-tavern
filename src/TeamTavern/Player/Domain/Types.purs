module TeamTavern.Player.Domain.Types where

import TeamTavern.Player.Domain.About (About)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Profile.Domain.Types (ByPlayerView)

type Secrets =
    { token :: Token
    , nonce :: Nonce
    }

type Identifiers =
    { email :: Email
    , nickname :: Nickname
    }

type IdentifiedToken =
    { id :: Id
    , token :: Token
    }

type AuthInfo =
    { id :: Id
    , nickname :: Nickname
    , token :: Token
    }

type NicknamedNonce =
    { nickname :: Nickname
    , nonce :: Nonce
    }

type NicknamedToken =
    { nickname :: Nickname
    , token :: Token
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
    { id :: Id
    , email :: Email
    , nickname :: Nickname
    , token :: Token
    , nonce :: Nonce
    }

type NicknamedAbout =
    { nickname :: Nickname
    , about :: About
    }

type View =
    { nickname :: Nickname
    , about :: About
    , profiles :: Array ByPlayerView
    }
