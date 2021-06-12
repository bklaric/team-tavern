module TeamTavern.Client.Script.LastUpdated (lastUpdated, daysToSignUp) where

import Prelude

import Data.Int (floor)
import Data.Maybe (Maybe(..))

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> (if count == 1 then "" else "s") <> " ago"
    Nothing -> "less than a minute ago"

daysToSignUp :: Number -> String
daysToSignUp signupDeadlineSeconds = let
    days = floor(signupDeadlineSeconds / daySeconds)
    in
    if days > 0
    then show days <> " day" <> (if days == 1 then "" else "s") <> " to sign up"
    else "Less than a day to sign up"
