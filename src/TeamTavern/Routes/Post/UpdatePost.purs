module TeamTavern.Routes.Post.UpdatePost where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Internal_, Literal, NoContent, NotAuthorized_, NotFound_, PutJson_)
import TeamTavern.Routes.Shared.Post (BadContent, RequestContent)

-- | Saving a post renews it (brief 9).
type UpdatePost =
    PutJson_ (Literal "games" / Capture "handle" String / Literal "own" / Capture "type" String) RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! NotFound_ ! Internal_
