module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON.Async (read)

type AddPlayerModel =
    { email :: Email
    , nickname :: Nickname
    , hash :: Hash
    }

type AddPlayerError errors errors' = TerrorVar
    ( InternalRow_
    + BadRequestRow (Variant (emailTaken :: {}, nicknameTaken :: {} | errors'))
    + errors )

queryString :: Query
queryString = Query """
    insert into player (email, nickname, password_hash)
    values ($1, $2, $3)
    returning id
    """

addPlayer :: ∀ querier errors errors'. Querier querier =>
    querier -> AddPlayerModel -> Async (AddPlayerError errors errors') Int
addPlayer pool { email, nickname, hash } = do
    result <- pool # query queryString (email : nickname :| hash) # lmap \error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key"
            || constraint error == Just "player_lower_email_key"
            -> Terror
                (badRequest_ $ inj (Proxy :: _ "emailTaken") {})
                ["Player email is taken: " <> show email, print error]
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
