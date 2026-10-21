module TeamTavern.Routes.Account.SwitchToDiscord where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NotAuthorized_, OkJson, PostJson_)

-- | Signs the account in with the Discord the code is for, in its password's,
-- | Steam's or Google's place, leaving its email as it is (brief 11.5).
type SwitchToDiscord =
    PostJson_ (Literal "account" / Literal "discord") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

-- | `code` is the one Discord sent the browser back to the sign-in page with.
type RequestContent = { code :: String }

-- | `contact` is the Discord username the account's posts now offer, where
-- | they offered none.
type OkContent = { contact :: Maybe String }

-- | `discordTaken` is a Discord that signs in to another account, and
-- | `discordRefused` a code Discord won't swap: spent, or given for another
-- | app or site.
type BadContent = Variant
    ( discordTaken :: {}
    , discordRefused :: {}
    )
