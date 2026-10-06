module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Foreign.Object (Object)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_)

type StartSession =
    PostJson_ (Literal "sessions") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Internal_)

type RequestContentEmail =
    { emailOrNickname :: String
    , password :: String
    }

type RequestContentDiscord = {accessToken :: String}

-- | `assertion` is the `openid.` parameters Steam sent the browser back with.
type RequestContentSteam = {assertion :: Object String}

type RequestContent = Variant
    ( password :: RequestContentEmail
    , discord :: RequestContentDiscord
    , steam :: RequestContentSteam
    )

-- | `unknownDiscord` carries the Discord username, which the nickname prompt
-- | offers to a player registering with Discord. `unknownSteam` carries the
-- | Steam profile name as a nickname, empty if Steam didn't give it, and the
-- | ticket the player registers that Steam account with. `steamRefused` is an
-- | answer Steam didn't give, gave for another site, or gave once already.
type BadContent = Variant
    ( unknownPlayer :: {}
    , wrongPassword :: {}
    , unknownDiscord :: { nickname :: String }
    , unknownSteam :: { nickname :: String, ticket :: String }
    , steamRefused :: {}
    )
