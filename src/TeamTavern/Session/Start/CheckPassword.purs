module TeamTavern.Session.Start.CheckPassword
    (PlayerId(..), CheckPasswordError, checkPassword) where

import Prelude

import Async (Async, left, note)
import Bcrypt.Async as Bcrypt
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Node.Errors as Node
import Postgres.Async.Query (query)
import Postgres.Error as Postgres
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Infrastructure.Cookie as Cookie
import TeamTavern.Session.Start.ReadModel (NicknameOrEmail, Password)

type CheckPasswordModel =
    { nicknameOrEmail :: NicknameOrEmail
    , password :: Password
    }

newtype PlayerId = PlayerId Int

derive instance newtypePlayerId :: Newtype PlayerId _

derive instance genericPlayerId :: Generic PlayerId _

instance showPlayerId :: Show PlayerId where show = genericShow

instance cookiePlayerId :: Cookie.PlayerId PlayerId where
    fromPlayerId (PlayerId playerId) = playerId

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
    select id, password_hash as hash
    from player
    where player.email = $1 or player.nickname = $1
    """

checkPasswordParameters :: NicknameOrEmail -> Array QueryParameter
checkPasswordParameters nicknameOrEmail =
    [unwrap nicknameOrEmail] <#> QueryParameter

checkPassword :: forall querier errors. Querier querier =>
    CheckPasswordModel -> querier -> Async (CheckPasswordError errors) PlayerId
checkPassword model @ { nicknameOrEmail, password } querier = do
    -- Load player hash.
    result <- querier
        # query checkPasswordQuery (checkPasswordParameters nicknameOrEmail)
        # label (SProxy :: SProxy "databaseError")
    hashes <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableHash")
            { result, errors: _ }
    { id, hash } :: { id :: Int, hash :: String } <- head hashes
        # note (inj (SProxy :: SProxy "noMatchingPlayer") nicknameOrEmail)

    -- Compare hash with password.
    matches <- Bcrypt.compare (unwrap password) hash
        # label (SProxy :: SProxy "bcryptError")

    when (not matches) $ left
        $ inj (SProxy :: SProxy "passwordDoesntMatch") nicknameOrEmail
    pure $ PlayerId id
