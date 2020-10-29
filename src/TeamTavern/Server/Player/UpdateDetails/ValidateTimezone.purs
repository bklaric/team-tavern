module TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone
    (Timezone, validateTimezone) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Timezones (allTimezones)

newtype Timezone = Timezone String

validateTimezone :: Maybe String -> Maybe Timezone
validateTimezone Nothing = Nothing
validateTimezone (Just timezone) =
    if allTimezones # Array.any \{ name } -> name == timezone
    then Just $ Timezone timezone
    else Nothing
