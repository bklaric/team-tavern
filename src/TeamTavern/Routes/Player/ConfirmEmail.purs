module TeamTavern.Routes.Player.ConfirmEmail where

import Jarilo (type (!), type (==>), Internal_, Literal, NoContent, NotFound_, PostJson_)

type ConfirmEmail =
    PostJson_ (Literal "confirm-email") RequestContent
    ==> (NoContent ! NotFound_ ! Internal_)

type RequestContent = {nonce :: String}
