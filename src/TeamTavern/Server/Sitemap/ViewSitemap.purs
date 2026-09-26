module TeamTavern.Server.Sitemap.ViewSitemap (viewSitemap) where

import Prelude

import Async (Async)
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

type Game = { handle :: String }

type Post = { handle :: String, id :: Int, updated :: String }

loadGamesQuery :: Query
loadGamesQuery = Query """
    select handle
    from game
    order by title
    """

-- An expired post is kept out of search engines until it is renewed.
loadPostsQuery :: Query
loadPostsQuery = Query """
    select
        game.handle,
        post.id,
        to_char(post.updated at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"') as updated
    from post
        join game on game.id = post.game_id
    where post.updated > now() - case when post.ilk = 'community'
        then interval '90 days' else interval '30 days' end
    order by game.title, post.updated desc
    """

loadGames :: ∀ errors. Pool -> Async (InternalTerror_ errors) (Array Game)
loadGames pool = queryMany_ pool loadGamesQuery

loadPosts :: ∀ errors. Pool -> Async (InternalTerror_ errors) (Array Post)
loadPosts pool = queryMany_ pool loadPostsQuery

-- A sitemap's addresses are absolute, and each origin the site answers on
-- (production, staging, the local stacks) lists its own. Caddy passes the
-- request's host on and says in `X-Forwarded-Proto` how it was reached.
origin :: Map String String -> String
origin headers =
    (Map.lookup "x-forwarded-proto" headers # fromMaybe "http")
    <> "://"
    <> (Map.lookup "host" headers # fromMaybe "")

escape :: String -> String
escape =
    replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")
    >>> replaceAll (Pattern "'") (Replacement "&apos;")

url :: String -> String -> Maybe String -> String
url origin' path lastmod = fold
    [ "<url><loc>", escape $ origin' <> path, "</loc>"
    , foldMap (\lastmod' -> "<lastmod>" <> lastmod' <> "</lastmod>") lastmod
    , "</url>\n"
    ]

sitemap :: String -> Array Game -> Array Post -> String
sitemap origin' games posts = fold
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
    , url origin' "/" Nothing
    , [ "/about", "/contact", "/terms", "/privacy" ] # foldMap \path -> url origin' path Nothing
    , games # foldMap \{ handle } -> url origin' ("/games/" <> handle) Nothing
    , posts # foldMap \{ handle, id, updated } ->
        url origin' ("/games/" <> handle <> "/posts/" <> show id) (Just updated)
    , "</urlset>\n"
    ]

viewSitemap :: ∀ left. Pool -> Map String String -> Async left _
viewSitemap pool headers =
    sendResponse "Error viewing sitemap" do
    games <- loadGames pool
    posts <- loadPosts pool
    pure $ ok_ $ sitemap (origin headers) games posts
