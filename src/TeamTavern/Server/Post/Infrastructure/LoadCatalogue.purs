module TeamTavern.Server.Post.Infrastructure.LoadCatalogue (Catalogue, loadCatalogue) where

import Prelude

import Async (Async, left)
import Data.Array (elem)
import Data.Bifunctor (lmap)
import Jarilo (notFound__)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Server.Country.ViewCountries (loadCountries)
import TeamTavern.Server.Game.ViewGame (loadGame)
import TeamTavern.Server.Infrastructure.Error (Terror(..), elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)

-- | What a post is checked against: its game with the game's fields and
-- | contacts, and the regions and countries.
type Catalogue =
    { gameId :: Int
    , game :: ViewGame.OkContent
    , countries :: ViewCountries.OkContent
    }

gameIdQuery :: Query
gameIdQuery = Query """
    select game.id from game where game.handle = $1
    """

loadCatalogue :: ∀ errors. Pool -> String -> String -> Async (LoadSingleError errors) Catalogue
loadCatalogue pool handle type_ = do
    unless (elem type_ [ "player", "group", "community" ]) $
        left $ Terror notFound__ [ "Unknown post type: " <> type_ ]
    { id } :: { id :: Int } <- queryFirstNotFound pool gameIdQuery (handle : [])
        # lmap (elaborate ("Can't find game: " <> handle))
    game <- loadGame pool handle
    countries <- loadCountries pool
    pure { gameId: id, game, countries }
