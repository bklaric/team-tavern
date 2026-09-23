module TeamTavern.Routes.Block.Block where

import Jarilo (type (!), type (/), type (==>), Capture, Internal_, Literal, NoBody, NoContent, NotAuthorized_, NotFound_, Post_)

-- | Blocks a player by nickname, hiding the two from each other both ways
-- | (brief 10). Blocking twice is no different from once. Not found is a
-- | nickname nobody has, or the viewer's own.
type Block =
    Post_ (Literal "blocks" / Capture "nickname" String) NoBody
    ==> NoContent ! NotAuthorized_ ! NotFound_ ! Internal_
