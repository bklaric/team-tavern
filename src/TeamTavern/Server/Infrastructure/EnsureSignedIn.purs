module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, SignedIn, ensureSignedIn) where

import Prelude

import Async (Async, left, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Jarilo (InternalRow_, NotAuthorizedRow_, notAuthorized__)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupToken)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Session.Domain.Token (Token, hash, sessionDays)
import Type.Row (type (+))

type EnsureSignedInError errors = TerrorVar (InternalRow_ + NotAuthorizedRow_ + errors)

type SignedIn = { id :: Id, token :: Token }

-- A session lapses when it goes unused for its days, and every use starts
-- them over.
queryString :: Query
queryString = Query """
    update session
    set last_used = current_timestamp
    where session.token_hash = $1
        and not session.revoked
        and session.last_used > current_timestamp - make_interval(days => $2)
    returning session.player_id as id
    """

-- A token the server refuses is answered like no token at all. The cookie
-- holding it does no harm, as nothing reads it but this, and the next sign-in
-- replaces it.
ensureSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (EnsureSignedInError errors) SignedIn
ensureSignedIn querier cookies =
    case lookupToken cookies of
    Nothing -> left $ Terror notAuthorized__ [ "No session token has been found in cookies." ]
    Just token -> do
        tokenHash <- hash token
        rows :: Array { id :: Int } <- queryMany querier queryString (tokenHash :| sessionDays)
        case rows of
            [ { id } ] -> right { id: wrap id, token }
            _ -> left $ Terror notAuthorized__ [ "The session token in cookies has no session." ]
