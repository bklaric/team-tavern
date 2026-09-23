module TeamTavern.Client.Script.Ago (ago, millisOf) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (round, toNumber)
import Data.Newtype (unwrap)

-- | An ISO time in milliseconds since the epoch.
foreign import millisOf :: String -> Number

unit' :: Int -> String -> String
unit' count name = show count <> " " <> name <> (if count == 1 then "" else "s") <> " ago"

-- | How long before now a time was, given as an ISO string, in the one unit
-- | that reads naturally: "just now", "5 minutes ago", "3 weeks ago".
ago :: Instant -> String -> String
ago now time = let
    minutes = round $ (unwrap (unInstant now) - millisOf time) / 60000.0
    hours = round $ toNumber minutes / 60.0
    days = round $ toNumber hours / 24.0
    in
    if minutes < 1 then "just now"
    else if minutes < 60 then unit' minutes "minute"
    else if hours < 24 then unit' hours "hour"
    else if days < 14 then unit' days "day"
    else if days < 60 then unit' (round $ toNumber days / 7.0) "week"
    else if days < 365 then unit' (round $ toNumber days / 30.0) "month"
    else unit' (round $ toNumber days / 365.0) "year"
