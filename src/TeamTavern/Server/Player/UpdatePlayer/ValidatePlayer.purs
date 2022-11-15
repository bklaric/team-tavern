module TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer where

import Prelude

import Async (Async)
import Async (fromEffect) as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Maybe (Maybe)
import TeamTavern.Routes.Player.UpdatePlayer as UpdatePlayer
import TeamTavern.Server.Player.UpdatePlayer.ValidateBirthday (validateOptionalBirthday)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLocation (Location, validateLocation)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone (Timezone, validateTimezone)

type Player =
    { birthday :: Maybe String
    , languages :: Array Language
    , location :: Maybe Location
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , microphone :: Boolean
    }

validatePlayer :: forall left. UpdatePlayer.RequestContent -> Async left Player
validatePlayer dto = do
    birthday <- Async.fromEffect $ validateOptionalBirthday dto.birthday
    let timezone = validateTimezone dto.timezone
    { birthday
    , languages: validateLanguages dto.languages
    , location: validateLocation dto.location
    , timezone
    , onlineWeekday: timezone >>= (const $ validateTimespan dto.weekdayFrom dto.weekdayTo)
    , onlineWeekend: timezone >>= (const $ validateTimespan dto.weekendFrom dto.weekendTo)
    , microphone: dto.microphone
    }
        # pure

validatePlayerV :: forall left. Semigroup left => UpdatePlayer.RequestContent -> AsyncV left Player
validatePlayerV = validatePlayer >>> AsyncV.fromAsync
