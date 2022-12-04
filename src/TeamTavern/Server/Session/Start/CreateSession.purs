module TeamTavern.Server.Session.Start.CreateSession (createSession) where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token (Token)

queryString :: Query
queryString = Query """
    insert into session (player_id, token)
    values ($1, $2)
    """

createSession :: forall querier errors. Querier querier =>
    Int -> Token -> querier -> Async (InternalTerror_ errors) Unit
createSession id token querier = queryNone querier queryString (id :| token)
