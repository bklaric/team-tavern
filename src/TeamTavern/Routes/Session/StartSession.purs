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

-- | `code` is the one Google sent the browser back to the sign-in page with.
type RequestContentGoogle = {code :: String}

type RequestContent = Variant
    ( password :: RequestContentEmail
    , discord :: RequestContentDiscord
    , steam :: RequestContentSteam
    , google :: RequestContentGoogle
    )

-- | `unknownDiscord` carries the Discord username, which the nickname prompt
-- | offers to a player registering with Discord. `unknownSteam` carries the
-- | Steam profile name as a nickname, empty if Steam didn't give it, and the
-- | ticket the player registers that Steam account with. `steamRefused` is an
-- | answer Steam didn't give, gave for another site, or gave once already.
-- | `unknownGoogle` and `googleRefused` are the same for Google, whose code
-- | is refused once spent or when it was given for another site.
type BadContent = Variant
    ( unknownPlayer :: {}
    , wrongPassword :: {}
    , unknownDiscord :: { nickname :: String }
    , unknownSteam :: { nickname :: String, ticket :: String }
    , steamRefused :: {}
    , unknownGoogle :: { nickname :: String, ticket :: String }
    , googleRefused :: {}
    )
