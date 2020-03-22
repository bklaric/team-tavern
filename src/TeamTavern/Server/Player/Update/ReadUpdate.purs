module TeamTavern.Server.Player.Update.ReadUpdate where

import Prelude

import Async (Async)
import Async (fromEither) as Async
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
import TeamTavern.Server.Player.Update.ValidateCountry (Country, validateOptionalCountry)
import TeamTavern.Server.Player.Update.ValidateDiscordTag (DiscordTag, DiscordTagError, validateOptionalDiscordTag)
import TeamTavern.Server.Player.Update.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.Update.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.Update.ValidateTimezone (Timezone, validateOptionalTimezone)

type UpdateDto =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , weekdayStart :: Maybe String
    , weekdayEnd :: Maybe String
    , weekendStart :: Maybe String
    , weekendEnd :: Maybe String
    , hasMicrophone :: Boolean
    }

type UpdateModel =
    { discordTag :: Maybe DiscordTag
    , birthday :: Maybe String
    , languages :: Array Language
    , country :: Maybe Country
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , hasMicrophone :: Boolean
    }

type UpdateModelError = Variant (discordTag :: NonEmptyList DiscordTagError)

type ReadUpdateError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: UpdateDto
        , errors :: NonEmptyList UpdateModelError
        }
    | errors )

readUpdate :: forall errors.
    Body -> Async (ReadUpdateError errors) UpdateModel
readUpdate body = do
    content <- readBody body
    dto @ { discordTag, birthday, languages, country, timezone, weekdayStart, weekdayEnd, weekendStart, weekendEnd, hasMicrophone } :: UpdateDto <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
        # Async.fromEither
    { discordTag: _
    , birthday
    , languages: validateLanguages languages
    , country: validateOptionalCountry country
    , timezone: validateOptionalTimezone timezone
    , onlineWeekday: validateTimespan weekdayStart weekdayEnd
    , onlineWeekend: validateTimespan weekendStart weekendEnd
    , hasMicrophone
    }
        <$> (validateOptionalDiscordTag discordTag
            # Validated.label (SProxy :: SProxy "discordTag"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
