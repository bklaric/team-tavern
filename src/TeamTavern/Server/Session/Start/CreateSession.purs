module TeamTavern.Server.Session.Start.CreateSession (createSession) where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Session.Domain.Token (Token)

type CreateSessionModel =
    { id :: Id
    , token :: Token
    }

queryString :: Query
queryString = Query """
    insert into session (player_id, token)
    values ($1, $2)
    """

createSession :: forall querier errors. Querier querier =>
    CreateSessionModel -> querier -> Async (InternalError errors) Unit
createSession { id, token } querier = queryNone querier queryString (id :| token)
