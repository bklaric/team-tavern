module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_)

type RegisterPlayer =
    PostJson_ (Literal "players") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Internal_)

-- | `ticket` is the one signing in with Steam or Google gave. Steam gives no
-- | address, so the player types one in; Google's rides on its ticket.
type RequestContent = Variant
    ( password ::
        { email :: String
        , nickname :: String
        , password :: String
        }
    , discord ::
        { nickname :: String
        , accessToken :: String
        }
    , steam ::
        { nickname :: String
        , email :: String
        , ticket :: String
        }
    , google ::
        { nickname :: String
        , ticket :: String
        }
    )

type RegistrationError = Variant
    ( email :: {}
    , nickname :: {}
    , password :: {}
    )

-- | `steamTicket` and `googleTicket` are a ticket past its hour or already
-- | spent.
type BadContent = Variant
    ( registration :: NonEmptyArray RegistrationError
    , emailTaken :: {}
    , nicknameTaken :: {}
    , discordTaken :: {}
    , steamTaken :: {}
    , steamTicket :: {}
    , googleTaken :: {}
    , googleTicket :: {}
    )
