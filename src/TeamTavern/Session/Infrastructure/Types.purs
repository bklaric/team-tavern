module TeamTavern.Session.Infrastructure.Types where

type NicknamedNonceModel =
    { nickname :: String
    , nonce :: String
    }

type IdentifiedTokenModel =
    { id :: Int
    , token :: String
    }
