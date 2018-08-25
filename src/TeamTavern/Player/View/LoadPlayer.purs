module TeamTavern.Player.View.LoadPlayer where

import Prelude

import Async (Async, fromEither)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON (read)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.View (About(..), PlayerView)

loadPlayerQuery :: Query
loadPlayerQuery = Query """
    select id, about
    from player
    where nickname = $1
    """

loadPlayerQueryParameters :: Nickname -> Array QueryParameter
loadPlayerQueryParameters nickname = [unwrap nickname] <#> QueryParameter

type LoadPlayerError =
    { nickname :: Nickname
    , error :: Variant
        ( notFound :: Unit
        , cantReadView :: Result
        , other :: Error
        )
    }

_notFound = SProxy :: SProxy "notFound"

_cantReadView = SProxy :: SProxy "cantReadView"

_other = SProxy :: SProxy "other"

_loadPlayer = SProxy :: SProxy "loadPlayer"

readView :: Nickname -> Result -> Async LoadPlayerError PlayerView
readView nickname result = fromEither $ lmap { error: _, nickname } do
    views <- rows result # traverse read
        # lmap (const $ inj _cantReadView result)
    { id, about } :: { id :: Int, about :: String } <- head views
        # note (inj _notFound unit)
    note (inj _cantReadView result) do
        id' <- PlayerId.create id
        about' <- About about # Just
        pure { id: id', nickname, about: about' }

loadPlayer
    :: forall errors
    .  Pool
    -> Nickname
    -> Async (Variant (loadPlayer :: LoadPlayerError | errors)) PlayerView
loadPlayer pool nickname = label _loadPlayer do
    result <- pool
        # query loadPlayerQuery (loadPlayerQueryParameters nickname)
        # label _other
        # lmap { error: _, nickname }
    readView nickname result
