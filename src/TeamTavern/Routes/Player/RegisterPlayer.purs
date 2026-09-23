module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_)

type RegisterPlayer =
    PostJson_ (Literal "players") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Internal_)

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
    )

type RegistrationError = Variant
    ( email :: {}
    , nickname :: {}
    , password :: {}
    )

type BadContent = Variant
    ( registration :: NonEmptyArray RegistrationError
    , emailTaken :: {}
    , nicknameTaken :: {}
    , discordTaken :: {}
    )
