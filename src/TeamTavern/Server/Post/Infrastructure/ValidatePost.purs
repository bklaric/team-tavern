module TeamTavern.Server.Post.Infrastructure.ValidatePost (ValidPost, validatePost) where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Array (all, any, elem, find, length, null)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Date (Date, exactDate)
import Data.Enum (toEnum)
import Data.Foldable (sequence_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Validated (Validated, invalid)
import Data.Validated as Validated
import Data.Variant (inj)
import Foreign.Object as Object
import Jarilo (badRequest_)
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Field (Field)
import TeamTavern.Routes.Shared.Post (AccountContent, BadContent, PostContent, PostError, RequestContent)
import TeamTavern.Server.Domain.Paragraph as Paragraph
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Shared.Languages (allLanguages)
import TeamTavern.Shared.Timezones (allTimezones)
import Type.Proxy (Proxy(..))

-- | A post as it is stored: its text as paragraphs, and only the fields its
-- | type has.
type ValidPost =
    { post :: PostContent
    , summary :: Array String
    , account :: AccountContent
    }

type Checked = Validated (Terror (NonEmptyArray PostError)) Unit

ensure :: Boolean -> PostError -> String -> Checked
ensure true _ _ = pure unit
ensure false error line = invalid $ Terror (Nea.singleton error) [ line ]

fieldError :: String -> PostError
fieldError key = inj (Proxy :: _ "field") { key }

blank :: Maybe String -> Maybe String
blank value = value <#> trim >>= \value' -> if value' == "" then Nothing else Just value'

paragraphs :: String -> Array String
paragraphs text = Paragraph.create text
    # (Validated.hush :: Validated (NonEmptyArray Unit) _ -> _)
    # maybe [] (map Paragraph.toString)

-- A post keeps only what its type has (the schema's checks): a player post has
-- none of a group's or a community's own facts, and a community no numbers.
normalized :: String -> RequestContent -> RequestContent
normalized type_ { post, account } =
    { post: post
        { options = Object.filter (not <<< null) post.options
        , ranges = Object.filter (\{ from, to } -> isJust from || isJust to) post.ranges
        , name = if player then Nothing else blank post.name
        , groupSize = if group then post.groupSize else Nothing
        , groupWantedFrom = if group then post.groupWantedFrom else Nothing
        , groupWantedTo = if group then post.groupWantedTo else Nothing
        , regions = if player then [] else post.regions
        , languages = if player then [] else post.languages
        , ageFrom = if player then Nothing else post.ageFrom
        , ageTo = if player then Nothing else post.ageTo
        , discordServer = if player then Nothing else blank post.discordServer
        , website = if player then Nothing else blank post.website
        }
    , account: account
        { country = blank account.country
        , birthday = blank account.birthday
        , timezone = blank account.timezone
        , contacts = account.contacts <#> trim # Object.filter (_ /= "")
        }
    }
    where
    player = type_ == "player"
    group = type_ == "group"

parseDate :: String -> Maybe Date
parseDate text = case split (Pattern "-") text <#> Int.fromString of
    [ Just year, Just month, Just day ] -> do
        year' <- toEnum year
        month' <- toEnum month
        day' <- toEnum day
        exactDate year' month' day'
    _ -> Nothing

isTime :: String -> Boolean
isTime text = case split (Pattern ":") text <#> Int.fromString of
    [ Just hours, Just minutes ] -> hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60
    _ -> false

inRange :: Int -> Int -> Maybe Int -> Boolean
inRange low high = maybe true \value -> value >= low && value <= high

ordered :: Maybe Int -> Maybe Int -> Boolean
ordered (Just from) (Just to) = from <= to
ordered _ _ = true

shorterThan :: Int -> Maybe String -> Boolean
shorterThan limit = maybe true \value -> String.length value <= limit

-- Answers to the game's fields: a field of the game that the type is asked,
-- with options of that field. A player gives a point on an ordered field, a
-- group or a community a range.
answerChecks :: Array Field -> String -> PostContent -> Array Checked
answerChecks fields type_ post =
    (Object.toUnfoldable post.options <#> \(Tuple key chosen) ->
        let field = asked key
        in ensure
            (field # maybe false \field' ->
                field'.ilk /= "boolean"
                && (not field'.ordered || type_ == "player")
                && all (isOption field') chosen
                && (field'.ilk == "multi" || length chosen <= 1))
            (fieldError key)
            ("Options don't answer field " <> key <> ": " <> joinWith ", " chosen))
    <> (Object.toUnfoldable post.ranges <#> \(Tuple key { from, to }) ->
        let field = asked key
        in ensure
            (field # maybe false \field' ->
                field'.ordered && type_ /= "player"
                && maybe true (isOption field') from && maybe true (isOption field') to)
            (fieldError key)
            ("Range doesn't answer field " <> key))
    <> (post.flags <#> \key ->
        ensure (asked key <#> (_.ilk >>> eq "boolean") # fromMaybe false) (fieldError key)
            ("Flag doesn't answer field " <> key))
    where
    asked key = find (\field -> field.key == key && elem type_ field.appliesTo) fields
    isOption field key = any (_.key >>> eq key) field.options

checks
    :: ViewGame.OkContent -> ViewCountries.OkContent -> String -> Date -> RequestContent -> Array String
    -> Array Checked
checks game countries type_ today { post, account } summary =
    answerChecks game.fields type_ post
    <>
    [ ensure (all (flip elem countries.regions) post.regions) (fieldError "regions")
        ("Unknown regions: " <> joinWith ", " post.regions)
    , ensure (all (flip elem allLanguages) post.languages) (fieldError "languages")
        ("Unknown languages: " <> joinWith ", " post.languages)
    , ensure (inRange 13 99 post.ageFrom && inRange 13 99 post.ageTo && ordered post.ageFrom post.ageTo)
        (fieldError "ages") "Ages are out of range."
    , ensure
        (all (inRange 1 30) [ post.groupSize, post.groupWantedFrom, post.groupWantedTo ]
            && ordered post.groupWantedFrom post.groupWantedTo)
        (fieldError "size") "Group numbers are out of range."
    , ensure (post.online # maybe true \{ from, to } -> isTime from && isTime to)
        (inj (Proxy :: _ "hours") {}) "Online hours aren't times."
    , ensure (isNothing post.online || isJust account.timezone) (inj (Proxy :: _ "hours") {})
        "Online hours have no timezone."
    , ensure (type_ /= "community" || isJust post.name) (inj (Proxy :: _ "name") {})
        "A community has no name."
    , ensure (shorterThan 50 post.name) (inj (Proxy :: _ "name") {}) "The name is too long."
    , ensure (type_ /= "community" || not null summary) (inj (Proxy :: _ "summary") {})
        "A community has no words."
    , ensure (String.length (joinWith "" summary) <= 2000) (inj (Proxy :: _ "summary") {})
        "The words are too long."
    , ensure (elem post.contactPreference preferences) (fieldError "contactPreference")
        ("Contact preference doesn't suit a " <> type_ <> " post: " <> post.contactPreference)
    , ensure (not (type_ == "community" && post.contactPreference == "discord") || isJust post.discordServer)
        (inj (Proxy :: _ "discordServer") {}) "Joining by Discord with no invite."
    , ensure (not (type_ == "community" && post.contactPreference == "website") || isJust post.website)
        (inj (Proxy :: _ "website") {}) "Joining by website with no website."
    , ensure (shorterThan 200 post.discordServer) (inj (Proxy :: _ "discordServer") {})
        "The Discord invite is too long."
    , ensure (shorterThan 200 post.website) (inj (Proxy :: _ "website") {}) "The website is too long."
    , ensure (post.contactPreference /= "offsite" || not Object.isEmpty account.contacts)
        (inj (Proxy :: _ "reach") {}) "Reached off site with no contact."
    , ensure (maybe true (\country -> any (_.name >>> eq country) countries.countries) account.country)
        (fieldError "location") ("Unknown country: " <> fromMaybe "" account.country)
    , ensure (all (flip elem allLanguages) account.languages) (fieldError "languages")
        ("Unknown languages: " <> joinWith ", " account.languages)
    , ensure (maybe true (\birthday -> parseDate birthday # maybe false (_ <= today)) account.birthday)
        (fieldError "birthday") ("Birthday isn't a past date: " <> fromMaybe "" account.birthday)
    , ensure (maybe true (\timezone -> any (_.name >>> eq timezone) allTimezones) account.timezone)
        (fieldError "timezone") ("Unknown timezone: " <> fromMaybe "" account.timezone)
    ]
    <> (Object.toUnfoldable account.contacts <#> \(Tuple kind value) ->
        ensure
            (elem kind game.contacts && String.length value <= (if kind == "discord" then 37 else 100))
            (inj (Proxy :: _ "contact") { kind })
            ("Contact isn't the game's or is too long: " <> kind))
    where
    preferences =
        if type_ == "community" then [ "discord", "website", "message" ] else [ "message", "offsite", "either" ]

-- | Checks a post against its game and the brief's rules (brief 6, step 3),
-- | naming every field that is wrong.
validatePost :: ∀ errors.
    ViewGame.OkContent -> ViewCountries.OkContent -> String -> Date -> RequestContent
    -> Async (BadRequestTerror BadContent errors) ValidPost
validatePost game countries type_ today content = let
    content' = normalized type_ content
    summary = paragraphs content'.post.summary
    in
    sequence_ (checks game countries type_ today content' summary)
    <#> (\_ -> { post: content'.post, summary, account: content'.account })
    # AsyncVal.fromValidated
    # lmap (map (inj (Proxy :: _ "post") >>> badRequest_))
