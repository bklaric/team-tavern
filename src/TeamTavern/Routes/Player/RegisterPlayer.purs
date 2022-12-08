module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Forbidden_, Internal_, Literal, NoContent, PostJson_)

type RegisterPlayer =
    PostJson_ (Literal "players") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Forbidden_ ! Internal_)

type RequestContent =
    { nickname :: String
    , password :: String
    }

type OkContent = { nickname :: String }

type RegistrationError = Variant
    ( nickname :: {}
    , password :: {}
    )

type BadContent = Variant
    ( registration :: NonEmptyArray RegistrationError
    , nicknameTaken :: {}
    )
