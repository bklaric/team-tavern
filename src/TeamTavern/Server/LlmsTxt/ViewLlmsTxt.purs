module TeamTavern.Server.LlmsTxt.ViewLlmsTxt (viewLlmsTxt) where

import Prelude

import Async (Async)
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import Data.Variant (Variant)
import Jarilo (InternalRow_, OkRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Server.Guide.Guides (guides)
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.RequestOrigin (requestOrigin)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Row (type (+))

type Game = { title :: String, handle :: String }

loadGamesQuery :: Query
loadGamesQuery = Query """
    select title, handle
    from game
    order by title
    """

loadGames :: ∀ errors. Pool -> Async (InternalTerror_ errors) (Array Game)
loadGames pool = queryMany_ pool loadGamesQuery

link :: String -> String -> String -> String
link origin name path = "- [" <> name <> "](" <> origin <> path <> ")\n"

-- | The summary is the site's meta description, and each game is named as its
-- | feed's heading names it.
llmsTxt :: String -> Array Game -> String
llmsTxt origin games = fold
    [ "# TeamTavern\n\n"
    , "> Find players, groups and communities for the games you play. "
    , "Post once, and we'll tell you when someone new fits.\n\n"
    , "## Games\n\n"
    , games # foldMap \{ title, handle } -> link origin (title <> " LFG") ("/games/" <> handle)
    , "\n## Guides\n\n"
    , link origin "Guides" "/guides"
    , guides # foldMap \{ heading, slug } -> link origin heading ("/guides/" <> slug)
    , "\n## About\n\n"
    , link origin "About" "/about"
    , link origin "Contact" "/contact"
    , link origin "Terms" "/terms"
    , link origin "Privacy" "/privacy"
    , link origin "Sitemap" "/sitemap.xml"
    ]

viewLlmsTxt :: ∀ left. Pool -> Map String String -> Async left (Variant (OkRow String + InternalRow_ + ()))
viewLlmsTxt pool headers =
    sendResponse "Error viewing llms.txt" do
    games <- loadGames pool
    pure $ ok_ $ llmsTxt (requestOrigin headers) games
