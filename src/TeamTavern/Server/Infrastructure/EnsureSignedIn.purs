module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, SignedIn, ensureSignedIn) where

import Prelude

import Async (Async, left, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Jarilo (InternalRow_, NotAuthorizedRow_, notAuthorized__)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupToken)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Session.Domain.Token (hash)
import Type.Row (type (+))

type EnsureSignedInError errors = TerrorVar (InternalRow_ + NotAuthorizedRow_ + errors)

type SignedIn = { id :: Id }

queryString :: Query
queryString = Query """
    select session.player_id as id
    from session
    where session.token_hash = $1
        and not session.revoked
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
        rows :: Array { id :: Int } <- queryMany querier queryString (tokenHash : [])
        case rows of
            [ { id } ] -> right { id: wrap id }
            _ -> left $ Terror notAuthorized__ [ "The session token in cookies has no session." ]
