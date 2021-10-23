module TeamTavern.Routes.Shared.PlayerProfile where

import Data.Maybe (Maybe)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)

type PlayerProfileRow fields =
    ( platforms :: Platforms
    , platform :: Platform
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , about :: Array String
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    | fields
    )

type PlayerProfileOpen fields = Record (PlayerProfileRow fields)

type PlayerProfile = PlayerProfileOpen ()
