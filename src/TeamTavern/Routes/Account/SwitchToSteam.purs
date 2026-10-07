module TeamTavern.Routes.Account.SwitchToSteam where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Foreign.Object (Object)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NotAuthorized_, OkJson, PostJson_)

-- | Signs the account in with the Steam account Steam vouched for, in place of
-- | its password, Discord or Google, leaving its email as it is (brief 11.5).
type SwitchToSteam =
    PostJson_ (Literal "account" / Literal "steam") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

-- | `assertion` is the `openid.` parameters Steam sent the browser back with.
type RequestContent = { assertion :: Object String }

-- | `contact` is the SteamID64 the account's posts now offer, where they
-- | offered no Steam account.
type OkContent = { contact :: Maybe String }

-- | `steamTaken` is a Steam account that signs in to another account, and
-- | `steamRefused` an answer Steam didn't give, gave for another site, or gave
-- | once already.
type BadContent = Variant
    ( steamTaken :: {}
    , steamRefused :: {}
    )
