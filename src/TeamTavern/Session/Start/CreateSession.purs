module TeamTavern.Session.Start.CreateSession
    (CreateSessionError, createSession) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Session.Start.CheckPassword (PlayerId)
import TeamTavern.Session.Start.GenerateToken (Token, unToken)

type CreateSessionModel =
    { playerId :: PlayerId
    , token :: Token
    }

type CreateSessionError errors = Variant
    ( databaseError :: Error
    , noSessionStarted ::
        { playerId :: PlayerId
        , token :: Token
        }
    | errors )

createSessionQuery :: Query
createSessionQuery = Query """
    insert into session (player_id, token)
    values ($1, $2)
    """

createSessionParameters :: CreateSessionModel -> Array QueryParameter
createSessionParameters { playerId, token } =
    [show $ unwrap playerId, unToken token]
    <#> QueryParameter

createSession :: forall querier errors. Querier querier =>
    CreateSessionModel -> querier -> Async (CreateSessionError errors) Unit
createSession model @ { playerId, token } querier = do
    result <- querier
        # query createSessionQuery (createSessionParameters model)
        # label (SProxy :: SProxy "databaseError")
    case rowCount result of
        1 -> pure unit
        _ -> left $ inj (SProxy :: SProxy "noSessionStarted")
            { playerId, token }
