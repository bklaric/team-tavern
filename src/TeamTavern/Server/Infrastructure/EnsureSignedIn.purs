module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor.Label (label)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)

type EnsureSignedInError errors = Variant
    ( noCookieInfo :: { cookies :: Cookies }
    , databaseError :: Error
    , invalidSession :: { cookieInfo :: CookieInfo }
    | errors )

queryString :: Query
queryString = Query """
    select session.id
    from session
    join player on player.id = session.player_id
    where player.id = $1
        and player.nickname = $2
        and session.token = $3
        and revoked = false
    """

ensureSignedIn :: forall errors.
    Pool -> Cookies -> Async (EnsureSignedInError errors) CookieInfo
ensureSignedIn pool cookies =
    case lookupCookieInfo cookies of
    Nothing -> Async.left $ inj (SProxy :: SProxy "noCookieInfo") { cookies }
    Just cookieInfo @ { id, nickname, token } -> do
        result <- pool
            # query queryString (id : nickname :| token)
            # label (SProxy :: SProxy "databaseError")
        if rowCount result == 0
        then Async.left $ inj (SProxy :: SProxy "invalidSession") { cookieInfo }
        else Async.right cookieInfo
