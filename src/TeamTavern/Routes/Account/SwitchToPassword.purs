module TeamTavern.Routes.Account.SwitchToPassword where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NoContent, NotAuthorized_, PutJson_)

-- | Signs the account in with its email and the password, in Discord's place,
-- | or changes the password of an account that has one (brief 11.5). `email`
-- | is asked of an account without an address, and ignored where it has one.
-- | Every other session of the account ends.
type SwitchToPassword =
    PutJson_ (Literal "account" / Literal "password") RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

type RequestContent =
    { password :: String
    , email :: Maybe String
    }

-- | `emailTaken` is the address, the account's or the one given, signing in to
-- | another account.
type BadContent = Variant
    ( password :: {}
    , email :: {}
    , emailTaken :: {}
    )
