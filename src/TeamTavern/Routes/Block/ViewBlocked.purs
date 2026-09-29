module TeamTavern.Routes.Block.ViewBlocked where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)

-- | The players the viewer has blocked, by nickname, for the account page
-- | (brief 11.5).
type ViewBlocked =
    Get_ (Literal "blocks")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

type OkContent = Array { nickname :: String }
