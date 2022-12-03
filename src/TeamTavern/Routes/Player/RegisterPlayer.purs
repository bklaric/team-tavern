module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), Literal, OkJson, PostJson_, BadRequestJson)

type RegisterPlayer =
    PostJson_ (Literal "players") RequestContent
    ==> (OkJson OkContent ! BadRequestJson BadContent)

type RequestContent =
    { nickname :: String
    , password :: String
    }

type OkContent = { nickname :: String }

type BadContentIdentifiers = Variant
    ( invalidNickname :: {}
    , invalidPassword :: {}
    )

type BadContent = Variant
    ( registration :: Array BadContentIdentifiers
    , nicknameTaken :: {}
    )
