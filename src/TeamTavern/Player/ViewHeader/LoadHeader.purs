module TeamTavern.Player.ViewHeader.LoadHeader
    ( PlayerId(..)
    , Nickname(..)
    , LoadPlayerHeaderResult
    , LoadHeaderError
    , loadHeader
    ) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)

newtype PlayerId = PlayerId Int

derive instance genericPlayerId :: Generic PlayerId _

instance showPlayerId :: Show PlayerId where show = genericShow

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

type LoadPlayerHeaderResult = { nickname :: Nickname }

type LoadHeaderError errors = Variant
    ( databaseError :: Error
    , unreadableHeader ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: PlayerId
    | errors )

queryString :: Query
queryString = Query """
    select nickname
    from player
    where id = $1
    """

queryParameters :: PlayerId -> Array QueryParameter
queryParameters (PlayerId id) = QueryParameter <$> [show id]

loadHeader :: forall errors.
    Pool -> PlayerId -> Async (LoadHeaderError errors) LoadPlayerHeaderResult
loadHeader pool id = do
    result <- pool
        # query queryString (queryParameters id)
        # label (SProxy :: SProxy "databaseError")
    headers <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableHeader") { result, errors: _ }
    { nickname } :: { nickname :: String } <- head headers
        # note (inj (SProxy :: SProxy "notFound") id)
    pure $ { nickname: Nickname nickname }
