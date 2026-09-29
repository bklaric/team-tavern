module TeamTavern.Routes.Notification.ReadNotifications where

import Jarilo (type (!), type (/), type (==>), Internal_, Literal, NoBody, NoContent, NotAuthorized_, Post_)

-- | Marks every notification of the signed-in player read (brief 11.3, Mark
-- | all read).
type ReadNotifications =
    Post_ (Literal "notifications" / Literal "read") NoBody
    ==> NoContent ! NotAuthorized_ ! Internal_
