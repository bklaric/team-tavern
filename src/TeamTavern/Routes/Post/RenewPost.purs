module TeamTavern.Routes.Post.RenewPost where

import Jarilo (type (!), type (/), type (==>), Capture, Internal_, Literal, NoBody, NoContent, NotAuthorized_, NotFound_, Post_)

-- | Renewing makes the post active again from now, expired or not (brief 9).
-- | Not found is a post that isn't the player's, or isn't the game's.
type RenewPost =
    Post_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "renew") NoBody
    ==> NoContent ! NotAuthorized_ ! NotFound_ ! Internal_
