module TeamTavern.Routes.Shared.PlayerBase where

type PlayerBaseRow fields =
    ( nickname :: String
    | fields
    )

type PlayerBaseOpen fields = Record (PlayerBaseRow fields)

type PlayerBase = PlayerBaseOpen ()
