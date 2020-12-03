module TeamTavern.Server.Infrastructure.EnsureSignedInAs where

import Prelude

import Async (Async)
import Async as Async
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Variant (SProxy(..), inj)
import Postgres.Query (class Querier)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn)

type EnsureSignedInAsError errors = EnsureSignedInError
    ( notAuthorized :: Array String
    | errors
    )

ensureSignedInAs :: forall errors querier. Querier querier =>
    querier -> Cookies -> String -> Async (EnsureSignedInAsError errors) CookieInfo
ensureSignedInAs querier cookies nickname = do
    cookieInfo <- ensureSignedIn querier cookies
    if toLower nickname == toLower (unwrap cookieInfo.nickname)
    then pure cookieInfo
    else Async.left $ inj (SProxy :: SProxy "notAuthorized")
        [ "Player signed in as " <> unwrap cookieInfo.nickname <> "."
        , "Required to be signed in as " <> nickname <> "."
        ]
