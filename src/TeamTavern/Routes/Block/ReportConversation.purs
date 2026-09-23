module TeamTavern.Routes.Block.ReportConversation where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NoContent, NotAuthorized_, NotFound_, PostJson_)
import TeamTavern.Routes.Shared.Report (Report)

-- | Reports the other side of a conversation, against the post it is about.
-- | Bad request is as for `ReportPost`. Not found is a conversation the viewer
-- | isn't a side of, or one hidden by a block.
type ReportConversation =
    PostJson_ (Literal "messages" / Capture "id" Int / Literal "report") Report
    ==> NoContent ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! Internal_
