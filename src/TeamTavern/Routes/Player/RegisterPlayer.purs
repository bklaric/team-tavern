module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Forbidden_, Internal_, Literal, NoContent, PostJson_)

type RegisterPlayer =
    PostJson_ (Literal "players") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Forbidden_ ! Internal_)

type RequestContent = Variant
    ( email ::
        { email :: String
        , nickname :: String
        , password :: String
        }
    , discord ::
        { nickname :: String
        , accessToken :: String
        }
    )

type OkContent = { nickname :: String }

type RegistrationError = Variant
    ( email :: {}
    , nickname :: {}
    , password :: {}
    )

type BadContent = Variant
    ( registration :: NonEmptyArray RegistrationError
    , emailTaken :: {}
    , nicknameTaken :: {}
    )
