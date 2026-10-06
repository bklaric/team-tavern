module TeamTavern.Server.Player.Infrastructure.InsertPlayer (InsertPlayerError, insertPlayer) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Query (class Querier, Query, QueryParameter)
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON.Async (read)

type InsertPlayerError other errors =
    TerrorVar (InternalRow_ + BadRequestRow (Variant (nicknameTaken :: {} | other)) + errors)

-- | Runs the insert, which returns the new player's id. A taken nickname is
-- | `nicknameTaken`, and `taken` names what else is taken by the unique
-- | constraint that says so.
insertPlayer :: ∀ querier other errors. Querier querier =>
    Array (Tuple String (Variant (nicknameTaken :: {} | other)))
    -> querier -> Query -> Array QueryParameter -> Async (InsertPlayerError other errors) Int
insertPlayer taken querier queryString parameters = do
    let nicknameTaken = inj (Proxy :: _ "nicknameTaken") {}
        takenBy = [ Tuple "player_nickname_key" nicknameTaken, Tuple "player_lower_nickname_key" nicknameTaken ]
            <> taken
    result <- querier # query queryString parameters # lmap \error ->
        case code error == unique_violation, constraint error >>= flip lookup takenBy of
            true, Just content -> Terror (badRequest_ content)
                [ "Taken: " <> fromMaybe "" (constraint error), print error ]
            _, _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__
        ["Expected player id in query result, got no rows."])
    row # (read :: _ -> _ _ { id :: Int })
        <#> _.id
        # lmap (\error -> Terror internal__ ["Error reading player id: " <> show error])
