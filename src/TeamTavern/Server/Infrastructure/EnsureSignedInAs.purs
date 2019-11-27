module TeamTavern.Server.Infrastructure.EnsureSignedInAs where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor.Label (label)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn)

type EnsureSignedInAsError errors = EnsureSignedInError
    ( nicknameDoesntMatch ::
        { nickname :: String
        , cookieInfo :: CookieInfo
        }
    | errors )

ensureSignedInAs :: forall errors querier. Querier querier =>
    querier -> Cookies -> String -> Async (EnsureSignedInAsError errors) CookieInfo
ensureSignedInAs querier cookies nickname = do
    cookieInfo <- ensureSignedIn querier cookies
    if toLower nickname == toLower (unwrap cookieInfo.nickname)
    then pure cookieInfo
    else Async.left $ inj (SProxy :: SProxy "nicknameDoesntMatch")
        { nickname, cookieInfo }
