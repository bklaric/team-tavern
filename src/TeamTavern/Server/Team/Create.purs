module TeamTavern.Server.Team.Create (TeamModel, TeamError, OkContent, BadContent, create) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async.Validated as Async
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, toCodePointArray, trim)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (VariantNel, VariantValidated)
import Data.Validated.Label as Validated
import Data.Variant (Variant, match)
import Effect (Effect, foreachE)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, ok_)
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
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
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

newtype Handle = Handle String

aPoint :: CodePoint
aPoint = codePointFromChar 'a'

aCapPoint :: CodePoint
aCapPoint = codePointFromChar 'A'

zPoint :: CodePoint
zPoint = codePointFromChar 'z'

zCapPoint :: CodePoint
zCapPoint = codePointFromChar 'Z'

onePoint :: CodePoint
onePoint = codePointFromChar '1'

ninePoint :: CodePoint
ninePoint = codePointFromChar '9'

dashPoint :: CodePoint
dashPoint = codePointFromChar '-'

underscorePoint :: CodePoint
underscorePoint = codePointFromChar '_'

apostrophePoint :: CodePoint
apostrophePoint = codePointFromChar '\''

isLetter :: CodePoint -> Boolean
isLetter point = (aPoint <= point && point <= zPoint) || (aCapPoint <= point && point <= zCapPoint)

isNumber :: CodePoint -> Boolean
isNumber point = onePoint <= point && point <= ninePoint

generateHandle :: Name -> Handle
generateHandle name =
    toString name
    # toCodePointArray
    <#> (\point ->
        if isLetter point || isNumber point || point == dashPoint || point == underscorePoint
        then Just point
        else if point == apostrophePoint
        then Nothing
        else Just underscorePoint)
    # Array.catMaybes
    # fromCodePointArray
    # Handle

queryString :: Query
queryString = Query """
    with similar_handle_count as (
        select count(*)
        from team
        where team.handle ilike ($2 || '%')
    ),
    unique_handle as (
        select $2 || (
            case
                when (select count from similar_handle_count) = 0
                then ''
                else '' || ((select count from similar_handle_count) + 1)
            end
        ) as handle
    )
    insert into team
        ( owner_id
        , handle
        , name
        , website
        , age_from
        , age_to
        , locations
        , languages
        , microphone
        , discord_server
        , timezone
        , weekday_from
        , weekday_to
        , weekend_from
        , weekend_to
        , about
        )
    values
        ( $1, (select handle from unique_handle)
        , $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16
        )
    returning team.handle;
    """

queryParameters :: Id -> Handle -> Team -> Array QueryParameter
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

addTeam :: forall querier errors. Querier querier =>
    querier -> Id -> Handle -> Team -> Async (InternalError errors) { handle :: String }
addTeam pool ownerId handle team =
    queryFirstInternal pool queryString (queryParameters ownerId handle team)

type CreateError = Variant (internal :: Array String, client :: Array String, team :: TeamErrors)

logError :: CreateError -> Effect Unit
logError = Log.logError "Error creating team" (internalHandler >>> clientHandler >>> teamHandler)

type OkContent = { handle :: String }

type BadContent = Array TeamError

sendResponse :: Async CreateError { handle :: String } -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , client: const badRequest__
        , team: badRequest_ <<< writeJSON <<< Array.fromFoldable
        }
    )
    (ok_ <<< writeJSON)

create :: forall left. Pool -> Body -> Cookies -> Async left Response
create pool body cookies =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedIn pool cookies
    content <- readJsonBody body
    team <- validateTeam content
    let generatedHandle = generateHandle team.name
    addTeam pool cookieInfo.id generatedHandle team
