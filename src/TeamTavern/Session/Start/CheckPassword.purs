module TeamTavern.Session.Start.CheckPassword
    (CheckPasswordError, CheckPasswordResult, checkPassword) where

import Prelude

import Async (Async, left, note)
import Bcrypt.Async as Bcrypt
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Node.Errors as Node
import Postgres.Async.Query (query)
import Postgres.Error as Postgres
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Password (Password)
import TeamTavern.Session.Domain.NicknameOrEmail (NicknameOrEmail)

type CheckPasswordModel =
    { nicknameOrEmail :: NicknameOrEmail
    , password :: Password
    }

type CheckPasswordDto =
    { id :: Int
    , nickname :: String
    , hash :: String
    , emailConfirmed :: Boolean
    }

type CheckPasswordResult =
    { id :: Id
    , nickname :: Nickname
    , emailConfirmed :: Boolean
    }

type CheckPasswordError errors = Variant
    ( databaseError :: Postgres.Error
    , unreadableHash ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , noMatchingPlayer :: NicknameOrEmail
    , bcryptError :: Node.Error
    , passwordDoesntMatch :: NicknameOrEmail
    | errors )

checkPasswordQuery :: Query
checkPasswordQuery = Query """
    select
        id, nickname, password_hash as hash, email_confirmed as "emailConfirmed"
    from player
    where player.email = $1 or player.nickname = $1
    """

checkPasswordParameters :: NicknameOrEmail -> Array QueryParameter
checkPasswordParameters nicknameOrEmail =
    [unwrap nicknameOrEmail] <#> QueryParameter

checkPassword
    :: forall querier errors
    .  Querier querier
    => CheckPasswordModel
    -> querier
    -> Async (CheckPasswordError errors) CheckPasswordResult
checkPassword model @ { nicknameOrEmail, password } querier = do
    -- Load player hash.
    result <- querier
        # query checkPasswordQuery (checkPasswordParameters nicknameOrEmail)
        # label (SProxy :: SProxy "databaseError")
    dtos <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableHash")
            { result, errors: _ }
    { id, nickname, hash, emailConfirmed } :: CheckPasswordDto <- head dtos
        # note (inj (SProxy :: SProxy "noMatchingPlayer") nicknameOrEmail)

    -- Compare hash with password.
    matches <- Bcrypt.compare (unwrap password) hash
        # label (SProxy :: SProxy "bcryptError")
    when (not matches) $ left
        $ inj (SProxy :: SProxy "passwordDoesntMatch") nicknameOrEmail

    pure $ { id: wrap id, nickname: wrap nickname, emailConfirmed }
