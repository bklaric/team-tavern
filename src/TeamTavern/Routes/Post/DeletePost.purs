module TeamTavern.Routes.Post.DeletePost where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Internal_, Literal, NoContent, NotAuthorized_, NotFound_)

-- | Deleting a post deletes its conversations for both sides.
type DeletePost =
    Delete_ (Literal "games" / Capture "handle" String / Literal "own" / Capture "type" String)
    ==> NoContent ! NotAuthorized_ ! NotFound_ ! Internal_
