module TeamTavern.Client.Script.Clock (clock) where

-- A time of day, given in minutes after midnight, as the viewer's locale writes
-- it: "19:00" or "7pm". No timezone name; the viewer knows their own.
foreign import clock :: Int -> String
