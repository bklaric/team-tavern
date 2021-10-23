module TeamTavern.Routes.Shared.PlayerDetails where

import Data.Maybe (Maybe)

type PlayerDetailsRow fields =
    ( age :: Maybe Int
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    | fields
    )

type PlayerDetailsOpen fields = Record (PlayerDetailsRow fields)

type PlayerDetails = PlayerDetailsOpen ()
