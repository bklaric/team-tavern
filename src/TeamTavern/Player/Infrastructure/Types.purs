module TeamTavern.Player.Infrastructure.Types where

type IdentifiersModel =
    { email :: String
    , nickname :: String
    }

type NicknamedAboutModel =
    { nickname :: String
    , about :: String
    }
