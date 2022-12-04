module TeamTavern.Routes.Shared.TeamProfile where

import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size)

type TeamProfileRow fields =
    ( size :: Size
    , allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , newOrReturning :: Boolean
    , about :: Array String
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    | fields
    )

type TeamProfileOpen fields = Record (TeamProfileRow fields)

type TeamProfile = TeamProfileOpen ()

type TeamProfileError = Variant
    ( platforms :: {}
    , about :: {}
    , ambitions :: {}
    )
