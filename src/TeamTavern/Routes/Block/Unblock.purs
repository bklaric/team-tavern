module TeamTavern.Routes.Block.Unblock where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Internal_, Literal, NoContent, NotAuthorized_)

-- | Takes back the viewer's block of a player, which brings back what it hid.
-- | A block the other player made stands.
type Unblock =
    Delete_ (Literal "blocks" / Capture "nickname" String)
    ==> NoContent ! NotAuthorized_ ! Internal_
