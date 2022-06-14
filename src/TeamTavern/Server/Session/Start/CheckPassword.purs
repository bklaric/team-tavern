module TeamTavern.Server.Session.Start.CheckPassword
    (CheckPasswordError, CheckPasswordResult, checkPassword) where

import Prelude

import Async (Async, left, note)
import Bcrypt.Async as Bcrypt
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Node.Errors as Node
import Postgres.Async.Query (query)
import Postgres.Error as Postgres
import Postgres.Query (class Querier, Query(..), (:))
import Postgres.Result (Result, rows)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read)

type CheckPasswordModel =
    { nickname :: String
    , password :: String
    }

type CheckPasswordDto =
    { id :: Int
    , nickname :: String
    , hash :: String
    }

type CheckPasswordResult =
    { id :: Id
    , nickname :: Nickname
    }

type CheckPasswordError errors = Variant
    ( databaseError :: Postgres.Error
    , unreadableHash ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , noMatchingPlayer :: String
    , bcrypt :: Node.Error
    , passwordDoesntMatch :: String
    | errors )

queryString :: Query
queryString = Query """
    select
        player.id,
        player.nickname,
        player.password_hash as hash
    from player
    where lower(player.nickname) = lower($1)
    """

checkPassword
    :: forall querier errors
    .  Querier querier
    => CheckPasswordModel
    -> querier
    -> Async (CheckPasswordError errors) CheckPasswordResult
checkPassword model querier = do
    -- Load player hash.
    result <- querier
        # query queryString (model.nickname : [])
        # label (Proxy :: _ "databaseError")
    dtos <- rows result
        # traverse read
        # labelMap (Proxy :: _ "unreadableHash")
            { result, errors: _ }
    { id, nickname, hash } :: CheckPasswordDto <- head dtos
        # note (inj (Proxy :: _ "noMatchingPlayer") model.nickname)

    -- Compare hash with password.
    matches <- Bcrypt.compare model.password hash # label (Proxy :: _ "bcrypt")
    when (not matches) $ left $ inj (Proxy :: _ "passwordDoesntMatch") nickname

    pure $ { id: wrap id, nickname: wrap nickname }
