module TeamTavern.Routes.Account.SwitchToGoogle where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NoContent, NotAuthorized_, PostJson_)

-- | Signs the account in with the Google account the code is for, in place of
-- | its password, Discord or Steam, leaving its email as it is (brief 11.5).
type SwitchToGoogle =
    PostJson_ (Literal "account" / Literal "google") RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

-- | `code` is the one Google sent the browser back to the sign-in page with.
type RequestContent = { code :: String }

-- | `googleTaken` is a Google account that signs in to another account, and
-- | `googleRefused` a code Google won't swap, spent or for another site.
type BadContent = Variant
    ( googleTaken :: {}
    , googleRefused :: {}
    )
