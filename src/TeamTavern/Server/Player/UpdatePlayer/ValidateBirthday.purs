module TeamTavern.Server.Player.UpdatePlayer.ValidateBirthday where

import Prelude

import Data.Date (day, month, year)
import Data.DateTime (date)
import Data.Either (hush)
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (FormatterCommand(..), unformat)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now (nowDate)

validateBirthday :: String -> Effect (Maybe String)
validateBirthday birthday = do
    -- Get current date.
    now <- nowDate
    let yearNow = fromEnum $ year now
    let monthNow = fromEnum $ month now
    let dayNow = fromEnum $ day now

    -- Parse birthday.
    let dateFormat = YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil
    case unformat dateFormat birthday # hush <#> date of
        Just birthday' -> let
            yearBirthday = fromEnum $ year birthday'
            monthBirthday = fromEnum $ month birthday'
            dayBirthday = fromEnum $ day birthday'
            in
            -- Check if difference is at least 13 years.
            if yearNow - yearBirthday > 13
            then pure $ Just birthday
            else if yearNow - yearBirthday == 13
            then
                case compare monthNow monthBirthday of
                GT -> pure $ Just birthday
                LT -> pure Nothing
                EQ ->
                    case compare dayNow dayBirthday of
                    GT -> pure $ Just birthday
                    LT -> pure Nothing
                    EQ -> pure $ Just birthday
            else pure Nothing
        Nothing -> pure Nothing

validateOptionalBirthday :: Maybe String -> Effect (Maybe String)
validateOptionalBirthday birthday =
    case birthday of
    Just birthday' -> validateBirthday birthday'
    Nothing -> pure Nothing
