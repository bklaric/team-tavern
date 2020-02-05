module TeamTavern.Server.Player.Update.ValidateTimezone
    (Timezone, validateTimezone, validateOptionalTimezone) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Timezones (allTimezones)

newtype Timezone = Timezone String

validateTimezone :: String -> Maybe Timezone
validateTimezone timezone =
    if allTimezones # Array.any \{ name } -> name == timezone
    then Just $ Timezone timezone
    else Nothing

validateOptionalTimezone :: Maybe String -> Maybe Timezone
validateOptionalTimezone timezone = timezone >>= validateTimezone
