module TeamTavern.Client.Shared.Facts (ageOn, dateText, parseDate, timezoneOptions, timezoneText) where

import Prelude

import Data.Array (index)
import Data.Date (Date, day, exactDate, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.Tuple (Tuple(..))
import TeamTavern.Client.Components.Input (Option)
import TeamTavern.Shared.Timezones (allTimezones)

-- How the facts on an account read where they are shown rather than asked
-- for.

months :: Array String
months =
    [ "January", "February", "March", "April", "May", "June"
    , "July", "August", "September", "October", "November", "December"
    ]

-- | A date input's value, `2000-06-15`.
parseDate :: String -> Maybe Date
parseDate text = case split (Pattern "-") text <#> Int.fromString of
    [ Just year', Just month', Just day' ] -> do
        year'' <- toEnum year'
        month'' <- toEnum month'
        day'' <- toEnum day'
        exactDate year'' month'' day''
    _ -> Nothing

-- | `2000-06-15` as "15 June 2000".
dateText :: String -> String
dateText value = case split (Pattern "-") value <#> Int.fromString of
    [ Just year', Just month', Just day' ] ->
        show day' <> " " <> fromMaybe "" (index months (month' - 1)) <> " " <> show year'
    _ -> value

-- | The age of someone born on the date, on `today`, or nothing for a date
-- | that hasn't come.
ageOn :: Date -> String -> Maybe Int
ageOn today birthday = parseDate birthday >>= \born -> let
    years = fromEnum (year today) - fromEnum (year born)
    birthdayCome = Tuple (month today) (day today) >= Tuple (month born) (day born)
    in
    if born > today then Nothing else Just if birthdayCome then years else years - 1

timezoneText :: String -> String
timezoneText = replaceAll (Pattern "_") (Replacement " ")

timezoneOptions :: Array Option
timezoneOptions = allTimezones <#> \{ name } -> { value: name, label: timezoneText name }
