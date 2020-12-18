module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (labelMap)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Query (class Querier, Query(..), (:|))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type AddPlayerModel =
    { nickname :: Nickname
    , hash :: Hash
    }

type AddPlayerError errors = Variant
    ( internal :: Array String
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Error
        }
    | errors )

queryString :: Query
queryString = Query """
    insert into player (nickname, password_hash)
    values ($1, $2)
    returning id
    """

addPlayer
    :: forall querier errors
    .  Querier querier
    => querier
    -> AddPlayerModel
    -> Async (AddPlayerError errors) Int
addPlayer pool model @ { nickname, hash } = do
    result <- pool
        # query queryString (nickname :| hash)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken") { nickname, error }
            true | constraint error == Just "player_lower_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken") { nickname, error }
            _ -> inj (SProxy :: SProxy "internal") [ "Error adding player: " <> print error ])
    id <- rows result
        # head
        # note (inj (SProxy :: SProxy "internal")
            [ "Error adding player. Got no rows when reading id." ])
        # Async.fromEither
    read id
        <#> (_.id :: { id :: Int } -> Int)
        # labelMap (SProxy :: SProxy "internal") \error ->
            [ "Error adding player. Error when reading id: " <> show error ]
