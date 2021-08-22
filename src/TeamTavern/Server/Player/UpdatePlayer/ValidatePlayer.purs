module TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer where

import Prelude

import Async (Async)
import Async (fromEffect) as Async
import Async.Validated (fromValidated) as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Server.Player.UpdatePlayer.ReadPlayer (PlayerModel)
import TeamTavern.Server.Player.UpdatePlayer.ValidateBirthday (validateOptionalBirthday)
import TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag (DiscordTag, validateDiscordTag)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLocation (Location, validateLocation)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone (Timezone, validateTimezone)

type Player =
    { discordTag :: Maybe DiscordTag
    , birthday :: Maybe String
    , languages :: Array Language
    , location :: Maybe Location
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , microphone :: Boolean
    }

type PlayerError = Variant
    ( discordTag :: Array String
    )

type PlayerErrors = NonEmptyList PlayerError

validatePlayer :: forall errors.
    PlayerModel -> Async (Variant (player :: PlayerErrors | errors)) Player
validatePlayer dto = do
    birthday' <- Async.fromEffect $ validateOptionalBirthday dto.birthday
    let timezone' = validateTimezone dto.timezone
    { discordTag: _
    , birthday: birthday'
    , languages: validateLanguages dto.languages
    , location: validateLocation dto.location
    , timezone: timezone'
    , onlineWeekday: timezone' >>= (const $ validateTimespan dto.weekdayFrom dto.weekdayTo)
    , onlineWeekend: timezone' >>= (const $ validateTimespan dto.weekendFrom dto.weekendTo)
    , microphone: dto.microphone
    }
        <$> validateDiscordTag dto.discordTag
        # Async.fromValidated
        # label (SProxy :: SProxy "player")

validatePlayerV :: forall errors.
    PlayerModel -> AsyncV (NonEmptyList (Variant (player :: PlayerErrors | errors))) Player
validatePlayerV = validatePlayer >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
