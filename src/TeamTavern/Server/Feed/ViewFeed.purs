module TeamTavern.Server.Feed.ViewFeed (viewFeed) where

import Prelude

import Async (Async)
import Data.Array (filter, head, last)
import Data.Bifunctor (lmap)
import Data.Foldable (elem)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Jarilo (InternalRow_, NotFoundRow_, OkRow, internal__, ok_)
import JavaScript.Date as Date
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Feed.ViewFeed as ViewFeed
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description)
import TeamTavern.Server.Feed.Feed (feedQuery)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Error (Terror(..), elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryFirstNotFound, queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Shared.Timezones (postgresTimezone)
import Type.Row (type (+))
import Yoga.JSON (writeImpl)
import Yoga.JSON.Async (read)

gameQuery :: Query
gameQuery = Query """
    select game.id from game where game.handle = $1
    """

-- The columns the query adds to each card's. Its counts are bigint, which
-- node-pg hands over as strings.
type Batch =
    { fits_count :: String
    , missing_one_count :: String
    , missing_more_count :: String
    , more :: Boolean
    , cursor :: ViewFeed.Cursor
    }

allTypes :: Array String
allTypes = [ "player", "group", "community" ]

-- A group or a community is looking for players, so it is shown players
-- (brief 4); a player sees what Showing asks for, or every type.
shownTypes :: ViewFeed.RequestContent -> Array String
shownTypes { description, showing }
    | description.type /= "player" = [ "player" ]
    | otherwise = case filter (flip elem allTypes) showing of
        [] -> allTypes
        types -> types

count :: String -> Int
count = Int.fromString >>> fromMaybe 0

kept :: ∀ value. (value -> Boolean) -> value -> Maybe value
kept keep value = if keep value then Just value else Nothing

hours :: Regex
hours = unsafeRegex """^([01]?[0-9]|2[0-3]):[0-5][0-9]$""" noFlags

-- What Feed.sql casts and computes with goes as none where it would fail
-- there, so a made-up description gets a feed rather than an error. An age
-- wanted is years taken from today's date, which a large one carries out of
-- range, and no player is past 150. Hours are kept only as the bar and posts
-- give them, "19:00", since one Postgres can't read fails its cast, and a zone
-- it doesn't know fails `at time zone`.
cleaned :: Description -> Description
cleaned description = description
    { ageFrom = description.ageFrom >>= kept age
    , ageTo = description.ageTo >>= kept age
    , online = description.online <#> \{ from, to } ->
        { from: from >>= kept (test hours), to: to >>= kept (test hours) }
    , timezone = description.timezone >>= postgresTimezone
    }
    where
    age value = value >= 0 && value <= 150

-- A cursor goes back as the feed gave it, so one with a time Postgres can't
-- read was made up, and goes as none.
validTimeQuery :: Query
validTimeQuery = Query """
    select pg_input_is_valid($1, 'timestamptz') as valid
    """

viewFeed :: ∀ left. Pool -> String -> Cookies -> ViewFeed.RequestContent
    -> Async left (Variant (OkRow ViewFeed.OkContent + NotFoundRow_ + InternalRow_ + ()))
viewFeed pool handle cookies request =
    sendResponse "Error viewing feed" do
    (_ :: { id :: Int }) <- queryFirstNotFound pool gameQuery (handle : [])
        # lmap (elaborate ("Can't find game: " <> handle))
    viewer <- checkSignedIn pool cookies <#> map (_.id >>> unwrap)
    now <- liftEffect Date.now
    cursor <- request.cursor # traverse (\given -> do
        { valid } :: { valid :: Boolean } <- queryFirstInternal pool validTimeQuery (given.updated : [])
        pure $ if valid then Just given else Nothing) <#> join
    rows :: Array Foreign <- queryMany pool feedQuery
        ( handle
        : toNullable viewer
        : writeImpl (cleaned request.description)
        : shownTypes request
        : toNullable (writeImpl <$> cursor)
        : now
        : [] )
    posts :: Array CardRow <- rows # traverse read # lmap readError
    batches :: Array Batch <- rows # traverse read # lmap readError
    pure $ ok_
        { posts
        , tiers: case head batches of
            Just batch ->
                { fits: count batch.fits_count
                , missingOne: count batch.missing_one_count
                , missingMore: count batch.missing_more_count
                }
            Nothing -> { fits: 0, missingOne: 0, missingMore: 0 }
        , more: head batches <#> _.more # fromMaybe false
        , cursor: last batches <#> _.cursor
        }
    where
    readError errors = Terror internal__ [ "Error reading feed row: " <> show errors ]
