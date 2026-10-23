module TeamTavern.Routes.Block.ReportConversation where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Report (Report, ReportOutcome)
import TeamTavern.Routes.Shared.Wait (Wait)

-- | Reports the other side of a conversation, against the post it is about.
-- | Bad request and too many requests are as for `ReportPost`. Not found is a
-- | conversation the viewer isn't a side of, or one hidden by a block.
type ReportConversation =
    PostJson_ (Literal "messages" / Capture "id" Int / Literal "report") Report
    ==> OkJson ReportOutcome ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! TooManyRequestsJson Wait ! Internal_
