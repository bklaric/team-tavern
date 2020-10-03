module TeamTavern.Server.Player.UpdateDetails.ReadUpdate where

import Prelude

import Async (Async)
import Async (fromEither, fromEffect) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Domain.Text (TextError)
import TeamTavern.Server.Player.Domain.About (About)
import TeamTavern.Server.Player.Domain.About as About
import TeamTavern.Server.Player.UpdateDetails.ValidateBirthday (validateOptionalBirthday)
import TeamTavern.Server.Player.UpdateDetails.ValidateCountry (Country, validateOptionalCountry)
import TeamTavern.Server.Player.UpdateDetails.ValidateDiscordTag (DiscordTag, DiscordTagError, validateOptionalDiscordTag)
import TeamTavern.Server.Player.UpdateDetails.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimezone (Timezone, validateOptionalTimezone)

type UpdateDetailsDto =
    { birthday :: Maybe String
    , country :: Maybe String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , discordTag :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type UpdateDetailsModel =
    { discordTag :: Maybe DiscordTag
    , birthday :: Maybe String
    , languages :: Array Language
    , country :: Maybe Country
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , hasMicrophone :: Boolean
    , about :: About
    }

type UpdateDetailsModelError = Variant
    ( discordTag :: NonEmptyList DiscordTagError
    , about :: NonEmptyList TextError
    )

type ReadUpdateError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: UpdateDetailsDto
        , errors :: NonEmptyList UpdateDetailsModelError
        }
    | errors )

readUpdate :: forall errors.
    Body -> Async (ReadUpdateError errors) UpdateDetailsModel
readUpdate body = do
    content <- readBody body
    dto :: UpdateDetailsDto <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
        # Async.fromEither
    birthday' <- Async.fromEffect $ validateOptionalBirthday dto.birthday
    let timezone' = validateOptionalTimezone dto.timezone
    { discordTag: _
    , birthday: birthday'
    , languages: validateLanguages dto.languages
    , country: validateOptionalCountry dto.country
    , timezone: timezone'
    , onlineWeekday: timezone' >>= (const $ validateTimespan dto.weekdayFrom dto.weekdayTo)
    , onlineWeekend: timezone' >>= (const $ validateTimespan dto.weekendFrom dto.weekendTo)
    , hasMicrophone: dto.hasMicrophone
    , about: _
    }
        <$> (validateOptionalDiscordTag dto.discordTag
            # Validated.label (SProxy :: SProxy "discordTag"))
        <*> (About.create dto.about
            # Validated.label (SProxy :: SProxy "about"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
