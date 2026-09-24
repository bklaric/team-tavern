module TeamTavern.Routes.Account.SwitchToDiscord where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NotAuthorized_, OkJson, PostJson_)

-- | Signs the account in with the Discord the access token is for, in its
-- | password's place, leaving its email as it is (brief 11.5).
type SwitchToDiscord =
    PostJson_ (Literal "account" / Literal "discord") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

type RequestContent = { accessToken :: String }

-- | `contact` is the Discord username the account's posts now offer, where
-- | they offered none.
type OkContent = { contact :: Maybe String }

-- | `discordTaken` is a Discord that signs in to another account.
type BadContent = Variant
    ( discordTaken :: {}
    )
