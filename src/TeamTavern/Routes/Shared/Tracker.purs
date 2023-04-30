module TeamTavern.Routes.Shared.Tracker where

import TeamTavern.Routes.Shared.Platform (Platform)

type Tracker =
    { platform :: Platform
    , title :: String
    , template :: String
    }

type Trackers = Array Tracker
