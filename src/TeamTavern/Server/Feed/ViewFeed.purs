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
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Jarilo (internal__, ok_)
import JavaScript.Date as Date
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Feed.ViewFeed as ViewFeed
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Server.Feed.Feed (feedQuery)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Error (Terror(..), elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
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

viewFeed :: ∀ left. Pool -> String -> Cookies -> ViewFeed.RequestContent -> Async left _
viewFeed pool handle cookies request =
    sendResponse "Error viewing feed" do
    (_ :: { id :: Int }) <- queryFirstNotFound pool gameQuery (handle : [])
        # lmap (elaborate ("Can't find game: " <> handle))
    viewer <- checkSignedIn pool cookies <#> map (_.id >>> unwrap)
    now <- liftEffect Date.now
    rows :: Array Foreign <- queryMany pool feedQuery
        ( handle
        : toNullable viewer
        : writeImpl request.description
        : shownTypes request
        : toNullable (writeImpl <$> request.cursor)
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
