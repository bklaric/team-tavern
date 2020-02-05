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
import TeamTavern.Server.Domain.Text (TextError)
import TeamTavern.Server.Player.Domain.About (About)
import TeamTavern.Server.Player.Domain.About as About
import TeamTavern.Server.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Server.Player.Domain.Nickname as Nickname
import TeamTavern.Server.Player.Update.ValidateCountry (Country, validateOptionalCountry)
import TeamTavern.Server.Player.Update.ValidateDiscordTag (DiscordTag, DiscordTagError, validateOptionalDiscordTag)
import TeamTavern.Server.Player.Update.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.Update.ValidateTimezone (Timezone, validateOptionalTimezone)

type UpdateDto =
    { nickname :: String
    , discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , hasMicrophone :: Boolean
    , about :: String
    , notify :: Boolean
    }

type UpdateModel =
    { nickname :: Nickname
    , discordTag :: Maybe DiscordTag
    , birthday :: Maybe String
    , languages :: Array Language
    , country :: Maybe Country
    , timezone :: Maybe Timezone
    , hasMicrophone :: Boolean
    , about :: About
    , notify :: Boolean
    }

type UpdateModelError = Variant
    ( nickname :: NonEmptyList NicknameError
    , discordTag :: NonEmptyList DiscordTagError
    , about :: NonEmptyList TextError
    )

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
    dto @ { nickname, discordTag, birthday, languages, country, timezone, hasMicrophone, about, notify } :: UpdateDto <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
        # Async.fromEither
    { nickname: _
    , discordTag: _
    , birthday
    , languages: validateLanguages languages
    , country: validateOptionalCountry country
    , timezone: validateOptionalTimezone timezone
    , hasMicrophone
    , about: _
    , notify
    }
        <$> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (validateOptionalDiscordTag discordTag
            # Validated.label (SProxy :: SProxy "discordTag"))
        <*> (About.create about
            # Validated.label (SProxy :: SProxy "about"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
