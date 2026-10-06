module TeamTavern.Server.Account.Infrastructure.SwitchSignIn (switchSignIn) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query, QueryParameter)
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines, transaction)
import TeamTavern.Server.Session.Domain.Token (Token)
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeOtherSessions)
import Type.Row (type (+))
import Yoga.JSON.Async (read)

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
