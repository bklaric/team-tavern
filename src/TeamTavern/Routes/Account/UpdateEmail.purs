module TeamTavern.Routes.Account.UpdateEmail where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NoContent, NotAuthorized_, PutJson_)

-- | A new address waits for the link sent to it, and the site sends it
-- | nothing else until then (brief 6, step 4). An address changed only in case
-- | is left as it is.
type UpdateEmail =
    PutJson_ (Literal "account" / Literal "email") RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

type RequestContent = { email :: String }

-- | `emailTaken` is an address another account signs in with.
type BadContent = Variant
    ( email :: {}
    , emailTaken :: {}
    )
