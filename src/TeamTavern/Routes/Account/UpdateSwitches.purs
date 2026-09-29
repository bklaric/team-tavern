module TeamTavern.Routes.Account.UpdateSwitches where

import Jarilo (type (!), type (/), type (==>), Internal_, Literal, NoContent, NotAuthorized_, PutJson_)
import TeamTavern.Routes.Account.ViewAccount (Switches)

-- | Which emails the player gets (brief 11.5).
type UpdateSwitches =
    PutJson_ (Literal "account" / Literal "switches") Switches
    ==> NoContent ! NotAuthorized_ ! Internal_
