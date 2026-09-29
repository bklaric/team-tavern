module TeamTavern.Routes.Post.CreatePost where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Internal_, Literal, NoContent, NotAuthorized_, NotFound_, PostJson_)
import TeamTavern.Routes.Shared.Post (BadContent, RequestContent)

type CreatePost =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "own" / Capture "type" String) RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! NotFound_ ! Internal_
