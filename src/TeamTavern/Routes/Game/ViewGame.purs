module TeamTavern.Routes.Game.ViewGame where

import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, OkJson, NotFound_)
import TeamTavern.Routes.Shared.Field (Fields)
import TeamTavern.Routes.Shared.Platform (Platforms)
import TeamTavern.Routes.Shared.Tracker (Trackers)

type ViewGame =
    Get_ (Literal "games" / Capture "handle" String)
    ==> OkJson OkContent ! NotFound_ ! Internal_

type OkContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , platforms :: Platforms
    , trackers :: Trackers
    , fields :: Fields
    }
