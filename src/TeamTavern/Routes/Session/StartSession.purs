module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_)

type StartSession =
    PostJson_ (Literal "sessions") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Internal_)

type RequestContentEmail =
    { emailOrNickname :: String
    , password :: String
    }

type RequestContentDiscord = {accessToken :: String}

type RequestContent = Variant
    ( password :: RequestContentEmail
    , discord :: RequestContentDiscord
    )

-- | `unknownDiscord` carries the Discord username, which the nickname prompt
-- | offers to a player registering with Discord.
type BadContent = Variant
    ( unknownPlayer :: {}
    , wrongPassword :: {}
    , unknownDiscord :: { nickname :: String }
    )
