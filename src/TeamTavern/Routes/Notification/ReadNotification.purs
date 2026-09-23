module TeamTavern.Routes.Notification.ReadNotification where

import Jarilo (type (!), type (/), type (==>), Capture, Internal_, Literal, NoBody, NoContent, NotAuthorized_, NotFound_, Post_)

-- | Marks one of the signed-in player's notifications read, as opening it does
-- | (brief 11.3). Not found for one that isn't theirs.
type ReadNotification =
    Post_ (Literal "notifications" / Capture "id" Int / Literal "read") NoBody
    ==> NoContent ! NotAuthorized_ ! NotFound_ ! Internal_
