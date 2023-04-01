module TeamTavern.Routes.Shared.PlayerProfile where

import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Field (Values)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)

type PlayerProfileRow fields =
    ( platforms :: Platforms
    , platform :: Platform
    , fieldValues :: Values
    , about :: Array String
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    | fields
    )

type PlayerProfileOpen fields = Record (PlayerProfileRow fields)

type PlayerProfile = PlayerProfileOpen ()

type PlayerProfileError = Variant
    ( about :: {}
    , ambitions :: {}
    )
