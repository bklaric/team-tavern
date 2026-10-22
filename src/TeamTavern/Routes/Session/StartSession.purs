module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Foreign.Object (Object)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Wait (Wait)

type StartSession =
    PostJson_ (Literal "sessions") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! TooManyRequestsJson Wait ! Internal_)

type RequestContentEmail =
    { emailOrNickname :: String
    , password :: String
    }

-- | `code` is the one Discord sent the browser back to the sign-in page with.
type RequestContentDiscord = {code :: String}

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
-- | offers to a player registering with Discord, and the ticket the player
-- | registers that Discord account with. `discordRefused` is a code Discord
-- | won't swap: spent, or given for another app or site. `unknownSteam`
-- | carries the Steam profile name as a nickname, empty if Steam didn't give
-- | it, and a ticket. `steamRefused` is an answer Steam didn't give, gave for
-- | another site, or gave once already. `unknownGoogle` and `googleRefused`
-- | are the same for Google, whose code is refused as Discord's is.
type BadContent = Variant
    ( unknownPlayer :: {}
    , wrongPassword :: {}
    , unknownDiscord :: { nickname :: String, ticket :: String }
    , discordRefused :: {}
    , unknownSteam :: { nickname :: String, ticket :: String }
    , steamRefused :: {}
    , unknownGoogle :: { nickname :: String, ticket :: String }
    , googleRefused :: {}
    )
