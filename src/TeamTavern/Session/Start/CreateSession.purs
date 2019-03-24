module TeamTavern.Session.Start.CreateSession
    (CreateSessionError, createSession) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:|))
import Postgres.Result (rowCount)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Session.Domain.Token (Token)

type CreateSessionModel =
    { id :: Id
    , token :: Token
    }

type CreateSessionError errors = Variant
    ( databaseError :: Error
    , noSessionStarted ::
        { id :: Id
        , token :: Token
        }
    | errors )

queryString :: Query
queryString = Query """
    insert into session (player_id, token)
    values ($1, $2)
    """

queryParameters :: CreateSessionModel -> Array QueryParameter
queryParameters { id, token } = id :| token

createSession :: forall querier errors. Querier querier =>
    CreateSessionModel -> querier -> Async (CreateSessionError errors) Unit
createSession model @ { id, token } querier = do
    result <- querier
        # query queryString (queryParameters model)
        # label (SProxy :: SProxy "databaseError")
    case rowCount result of
        1 -> pure unit
        _ -> left $ inj (SProxy :: SProxy "noSessionStarted")
            { id, token }
