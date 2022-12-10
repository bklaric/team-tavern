module TeamTavern.Server.Infrastructure.EnsureSignedInAs where

import Prelude

import Async (Async)
import Async as Async
import Data.Newtype (unwrap)
import Data.String (toLower)
import Jarilo (ForbiddenRow_, forbidden__)
import Postgres.Query (class Querier)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import Type.Row (type (+))

type EnsureSignedInAsError errors = EnsureSignedInError (ForbiddenRow_ + errors)

ensureSignedInAs :: ∀ errors querier. Querier querier =>
    querier -> Cookies -> String -> Async (EnsureSignedInAsError errors) CookieInfo
ensureSignedInAs querier cookies nickname = do
    cookieInfo <- ensureSignedIn querier cookies
    if toLower nickname == toLower (unwrap cookieInfo.nickname)
    then pure cookieInfo
    else Async.left $ Terror forbidden__
        [ "Player signed in as " <> unwrap cookieInfo.nickname <> "."
        , "Required to be signed in as " <> nickname <> "."
        ]
