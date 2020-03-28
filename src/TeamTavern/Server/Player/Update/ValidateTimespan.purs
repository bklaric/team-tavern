module TeamTavern.Server.Player.Update.ValidateTimespan
    (Hours, Minutes, Time, Timespan, toString, startTime, endTime, validateTimespan) where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace, split)
import Data.String.Utils (padStart)

type Hours = Int

type Minutes = Int

data Time = Time Hours Minutes

derive instance eqTime :: Eq Time

instance ordTime :: Ord Time where
    compare (Time hoursLeft minutesLeft) (Time hoursRight minutesRight) =
        case compare hoursLeft hoursRight of
        GT -> GT
        EQ -> compare minutesLeft minutesRight
        LT -> LT

toString :: Time -> String
toString (Time hours minutes) =
    (replace (Pattern " ") (Replacement "0") $ padStart 2 $ show hours)
    <> ":"
    <> (replace (Pattern " ") (Replacement "0") $ padStart 2 $ show minutes)

data Timespan = Timespan Time Time

startTime :: Timespan -> Time
startTime (Timespan time _) = time

endTime :: Timespan -> Time
endTime (Timespan _ time) = time

validateTime :: String -> Maybe Time
validateTime time =
    case split (Pattern ":") time of
    [hours, minutes] -> do
        hours' <- fromString hours
        minutes' <- fromString minutes
        let hoursValid = 0 <= hours' && hours' <= 23
        let minutesValid = 0 <= minutes' && minutes' <= 59
        if hoursValid && minutesValid
            then Just $ Time hours' minutes'
            else Nothing
    _ -> Nothing

validateTimespan :: Maybe String -> Maybe String -> Maybe Timespan
validateTimespan start end =
    Timespan <$> (start >>= validateTime) <*> (end >>= validateTime)
