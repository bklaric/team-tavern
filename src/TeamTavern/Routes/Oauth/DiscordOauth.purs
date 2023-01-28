module TeamTavern.Routes.Oauth.DiscordOauth where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequest, Forbidden_, Internal_, JsonBody, Literal, NoContent, NotFound_, Post_)

type DiscordOauth =
    Post_ (Literal "oauth" / Literal "discord") (JsonBody RequestContent)
    ==> (NoContent ! BadRequest (JsonBody BadContent) ! Forbidden_ ! NotFound_ ! Internal_)

type RequestContent = {nickname :: String, accessToken :: String}

type BadContent = Variant
    ( nickname :: {}
    , nicknameTaken :: {}
    )
