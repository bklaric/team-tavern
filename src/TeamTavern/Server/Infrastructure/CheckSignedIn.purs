module TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn) where

import Prelude

import Async (Async, attempt)
import Data.Either (hush)
import Data.Maybe (Maybe)
import JavaScript.Npm.Pg.Query (class Querier)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (SignedIn, ensureSignedIn)

-- | The player signed in, for a route anyone may ask. A session that can't be
-- | read is taken for none.
checkSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async errors (Maybe SignedIn)
checkSignedIn querier cookies = ensureSignedIn querier cookies # attempt <#> hush
