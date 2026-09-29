module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn) where

import Prelude

import Async (Async, note)
import Jarilo (InternalRow_, NotAuthorizedRow_, notAuthorized__)
import JavaScript.Npm.Pg.Query (class Querier)
import TeamTavern.Server.Infrastructure.CheckSignedIn (SignedIn, checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import Type.Row (type (+))

type EnsureSignedInError errors = TerrorVar (InternalRow_ + NotAuthorizedRow_ + errors)

ensureSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (EnsureSignedInError errors) SignedIn
ensureSignedIn querier cookies =
    checkSignedIn querier cookies
    >>= note (Terror notAuthorized__ [ "No session has been found for the request." ])
