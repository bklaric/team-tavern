module TeamTavern.Server.Infrastructure.CheckSignedIn (SignedIn, checkSignedIn) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupToken)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Session.Domain.Token (Token, hash, sessionDays)

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

-- | The player signed in, for a route anyone may ask. A token the server
-- | refuses is taken like no token at all. The cookie holding it does no harm,
-- | as nothing reads it but this, and the next sign-in replaces it.
checkSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (InternalTerror_ errors) (Maybe SignedIn)
checkSignedIn querier cookies =
    case lookupToken cookies of
    Nothing -> pure Nothing
    Just token -> do
        tokenHash <- hash token
        rows :: Array { id :: Int } <- queryMany querier queryString (tokenHash :| sessionDays)
        pure case rows of
            [ { id } ] -> Just { id: wrap id, token }
            _ -> Nothing
