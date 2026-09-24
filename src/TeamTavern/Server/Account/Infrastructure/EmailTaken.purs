module TeamTavern.Server.Account.Infrastructure.EmailTaken (EmailTakenError, emailTakenOrInternal) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error as Postgres
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type EmailTakenError errors errors' = TerrorVar
    ( InternalRow_
    + BadRequestRow (Variant (emailTaken :: {} | errors'))
    + errors )

-- | An address that another account signs in with is refused by
-- | `player_lower_email_key`, which holds only among accounts with a password,
-- | so it fires when an account takes the address or takes a password.
emailTakenOrInternal :: ∀ errors errors'. String -> Postgres.Error -> EmailTakenError errors errors'
emailTakenOrInternal email error =
    case code error == unique_violation, constraint error of
        true, Just "player_lower_email_key" -> Terror
            (badRequest_ $ inj (Proxy :: _ "emailTaken") {})
            [ "Another account signs in with email: " <> email, print error ]
        _, _ -> Terror internal__ $ databaseErrorLines error
