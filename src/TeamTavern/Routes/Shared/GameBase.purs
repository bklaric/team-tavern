module TeamTavern.Routes.Shared.GameBase where

type GameBaseRow fields =
    ( handle :: String
    , title :: String
    | fields
    )

type GameBaseOpen fields = Record (GameBaseRow fields)

type GameBase = GameBaseOpen ()
