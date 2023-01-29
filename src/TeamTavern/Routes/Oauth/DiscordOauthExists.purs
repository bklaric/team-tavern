module TeamTavern.Routes.Oauth.DiscordOauthExists where

import Jarilo (type (!), type (/), type (==>), Forbidden_, Internal_, JsonBody, Literal, NotFound_, Ok, Post_)

type DiscordOauthExists =
    Post_ (Literal "oauth" / Literal "discord" / Literal "exists") (JsonBody RequestContent)
    ==> (Ok (JsonBody OkContent) ! Forbidden_ ! NotFound_ ! Internal_)

type RequestContent = {accessToken :: String}

type OkContent = {exists :: Boolean}
