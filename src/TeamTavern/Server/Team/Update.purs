module TeamTavern.Server.Team.Update (TeamModel, TeamError, OkContent, BadContent, update) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async.Validated as Async
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (VariantNel, VariantValidated)
import Data.Validated.Label as Validated
import Data.Variant (Variant, match)
import Effect (Effect, foreachE)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (writeJSON)
import TeamTavern.Server.Domain.Text (Text, TextErrors, validateText)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logLines)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdateDetails.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (Timespan, nullableTimeFrom, nullableTimeTo, validateTimespan)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimezone (Timezone, validateTimezone)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (AgeSpan, nullableAgeFrom, nullableAgeTo, validateAgeSpan)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateRegions (Region, validateRegions)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlErrors, validateUrlV, validateUrlV_)
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

type TeamModel =
    { name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

-- Name

newtype Name = Name String

maxLength :: Int
maxLength = 40

type NameErrors = VariantNel (empty :: Empty, notPrintable :: NotPrintable, tooLong :: TooLong)

validateName :: forall errors. String -> VariantValidated (name :: Array String | errors) Name
validateName name
    = Wrapped.create trim [empty, tooLong maxLength, notPrintable] Name name
    # Validated.labelMap (SProxy :: SProxy "name") \(errors :: NameErrors) ->
        [ "Error validating name: " <> show errors ]

toString :: Name -> String
toString (Name name) = name

-- Website

validateWebsite :: forall errors.
    Maybe String -> VariantValidated (website :: Array String | errors) (Maybe Url)
validateWebsite website
    = website
    # traverse validateUrlV_
    # Validated.labelMap (SProxy :: SProxy "website") \(errors :: UrlErrors) ->
        [ "Error validating website: " <> show errors]

-- Discord server

validateDiscordServer :: forall errors.
    Maybe String -> VariantValidated (discordServer :: Array String | errors) (Maybe Url)
validateDiscordServer discordServer
    = discordServer
    # traverse (validateUrlV "discord.gg")
    # Validated.labelMap (SProxy :: SProxy "discordServer") \(errors :: UrlErrors) ->
        [ "Error validating Discord server: " <> show errors]

-- About

validateAbout :: forall errors. String -> VariantValidated (about :: Array String | errors) Text
validateAbout about
    = validateText about
    # Validated.labelMap (SProxy :: SProxy "about") \(errors :: TextErrors) ->
        [ "Error validating about text: " <> show errors ]

type Team =
    { name :: Name
    , website :: Maybe Url
    , ageSpan :: AgeSpan
    , locations :: Array Region
    , languages :: Array Language
    , microphone :: Boolean
    , discordServer :: Maybe Url
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , about :: Text
    }

type TeamError = Variant
    ( name :: Array String
    , website :: Array String
    , discordServer :: Array String
    , about :: Array String
    )

type TeamErrors = NonEmptyList TeamError

teamHandler :: forall fields. Lacks "team" fields =>
    Builder (Record fields) { team :: TeamErrors -> Effect Unit | fields }
teamHandler = Builder.insert (SProxy :: SProxy "team") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { name: logLines, website: logLines, discordServer: logLines, about: logLines }

validateTeam :: forall errors. TeamModel -> Async (Variant (team :: TeamErrors | errors)) Team
validateTeam (team :: TeamModel) = let
    name = validateName team.name
    website = validateWebsite team.website
    ageSpan = validateAgeSpan team.ageFrom team.ageTo
    locations = validateRegions team.locations
    languages = validateLanguages team.languages
    microphone = team.microphone
    discordServer = validateDiscordServer team.discordServer
    timezone = validateTimezone team.timezone
    onlineWeekday = timezone >>= (const $ validateTimespan team.weekdayFrom team.weekdayTo)
    onlineWeekend = timezone >>= (const $ validateTimespan team.weekendFrom team.weekendTo)
    about = validateAbout team.about
    in
    { name: _, website: _
    , ageSpan, locations
    , languages, microphone, discordServer: _
    , timezone, onlineWeekday, onlineWeekend
    , about: _
    }
    <$> name <*> website <*> discordServer <*> about
    # Async.fromValidated # label (SProxy :: SProxy "team")

queryString :: Query
queryString = Query """
    update team
    set
        name = $3,
        website = $4,
        age_from = $5,
        age_to = $6,
        locations = $7,
        languages = $8,
        microphone = $9,
        discord_server = $10,
        timezone = $11,
        weekday_from = $12,
        weekday_to = $13,
        weekend_from = $14,
        weekend_to = $15,
        about = $16
    where owner_id = $1 and handle = $2
    """

queryParameters :: Id -> String -> Team -> Array QueryParameter
queryParameters ownerId handle team
    = ownerId
    : handle
    : team.name
    : toNullable team.website
    : nullableAgeFrom team.ageSpan
    : nullableAgeTo team.ageSpan
    : team.locations
    : team.languages
    : team.microphone
    : toNullable team.discordServer
    : toNullable team.timezone
    : nullableTimeFrom team.onlineWeekday
    : nullableTimeTo team.onlineWeekday
    : nullableTimeFrom team.onlineWeekend
    : nullableTimeTo team.onlineWeekend
    :| team.about

updateTeam :: forall querier errors. Querier querier =>
    querier -> Id -> String -> Team -> Async (InternalError errors) Unit
updateTeam pool ownerId handle team =
    queryNone pool queryString (queryParameters ownerId handle team)

type CreateError = Variant (internal :: Array String, client :: Array String, team :: TeamErrors)

logError :: CreateError -> Effect Unit
logError = Log.logError "Error creating team" (internalHandler >>> clientHandler >>> teamHandler)

type OkContent = { handle :: String }

type BadContent = Array TeamError

sendResponse :: Async CreateError Unit -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , client: const badRequest__
        , team: badRequest_ <<< writeJSON <<< Array.fromFoldable
        }
    )
    (const $ noContent_)

update :: forall left. Pool -> Body -> Cookies -> { handle :: String } -> Async left Response
update pool body cookies { handle } =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedIn pool cookies
    content <- readJsonBody body
    team <- validateTeam content
    updateTeam pool cookieInfo.id handle team
