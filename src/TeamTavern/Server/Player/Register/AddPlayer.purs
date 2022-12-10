module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async, note)
import Async as Async
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Query (class Querier, Query(..), (:|))
import Postgres.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON (read)

type AddPlayerModel =
    { nickname :: Nickname
    , hash :: Hash
    }

type AddPlayerError errors errors' = TerrorVar
    ( InternalRow_
    + BadRequestRow (Variant (nicknameTaken :: {} | errors'))
    + errors )

queryString :: Query
queryString = Query """
    insert into player (nickname, password_hash)
    values ($1, $2)
    returning id
    """

addPlayer :: ∀ querier errors errors'. Querier querier =>
    querier -> AddPlayerModel -> Async (AddPlayerError errors errors') Int
addPlayer pool { nickname, hash } = do
    result <- pool # query queryString (nickname :| hash) # lmap \error ->
        case code error == unique_violation of
        true | constraint error == Just "player_nickname_key"
            || constraint error == Just "player_lower_nickname_key"
            -> Terror
                (badRequest_ $ inj (Proxy :: _ "nicknameTaken") {})
                ["Player nickname is taken: " <> show nickname, print error]
        _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__
        ["Expected player id in query result, got no rows."])
    row # (read :: _ -> _ _ { id :: Int })
        <#> _.id
        # lmap (\error -> Terror internal__
            ["Error reading player id: " <> show error])
        # Async.fromEither
