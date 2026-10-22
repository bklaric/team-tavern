module TeamTavern.Server.Account.Infrastructure.SwitchSignIn (askProvider, createSwitchLimit, switchSignIn) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Jarilo (BadRequestRow, InternalRow_, TooManyRequestsRow, badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query, QueryParameter)
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines, transaction)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter, countFailures, createLimiter)
import TeamTavern.Server.Session.Domain.Token (Token)
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeOtherSessions)
import Type.Row (type (+))
import Yoga.JSON.Async (read)

-- | A switch to Discord, Steam or Google asks the provider from the site's own
-- | address, which the provider holds back for everyone once it has refused
-- | too much from it, so an account tries 10 switches in 10 minutes that the
-- | provider doesn't vouch for.
createSwitchLimit :: Effect Limiter
createSwitchLimit = createLimiter "Failed sign-in switches of one account" { count: 10, seconds: 600 }

-- | Asks the provider with `ask`, if the account has room for one more it
-- | doesn't vouch for.
askProvider :: ∀ result errors.
    Limiter -> Int
    -> Async (TerrorVar (TooManyRequestsRow Wait + errors)) result
    -> Async (TerrorVar (TooManyRequestsRow Wait + errors)) result
askProvider limit playerId =
    countFailures limit (show playerId) "Too many failed sign-in switches of one account."

-- | Runs the update that moves the player's sign-in, which returns the contact
-- | it filled in where the account's posts offered none, and ends every session
-- | but the one `token` names. Another account signing in the same way breaks
-- | `constraint`, and is `taken`.
switchSignIn :: ∀ body errors.
    Pool -> Int -> Token -> Query -> Array QueryParameter -> { constraint :: String, taken :: body }
    -> Async (TerrorVar (InternalRow_ + BadRequestRow body + errors)) { contact :: Maybe String }
switchSignIn pool playerId token queryString parameters { constraint: takenConstraint, taken } =
    pool # transaction \client -> do
        result <- client # query queryString parameters
            # lmap \error -> case code error == unique_violation, constraint error of
                true, Just name | name == takenConstraint -> Terror (badRequest_ taken)
                    [ "Another account signs in this way: " <> name, print error ]
                _, _ -> Terror internal__ $ databaseErrorLines error
        row <- result # rows # head # note (Terror internal__ [ "Expected the switched player, got no rows." ])
        contact <- read row # lmap \error -> Terror internal__ [ "Error reading the filled contact: " <> show error ]
        revokeOtherSessions client playerId token
        pure contact
