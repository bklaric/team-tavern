module TeamTavern.Client.Script.Day (dayLabel, timeOfDay) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Newtype (unwrap)
import TeamTavern.Client.Script.Clock (clock)

foreign import dayLabelImpl :: Number -> String -> String

foreign import minuteOfDay :: String -> Int

-- | The viewer's day an ISO time falls on, as a thread heads it: "Today",
-- | "Yesterday" or "12 March".
dayLabel :: Instant -> String -> String
dayLabel now = dayLabelImpl (unwrap $ unInstant now)

-- | The viewer's time of day of an ISO time, as their locale writes it.
timeOfDay :: String -> String
timeOfDay = minuteOfDay >>> clock
