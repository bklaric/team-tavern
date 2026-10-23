module TeamTavern.Routes.Block.ReportPost where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Report (Report, ReportOutcome)
import TeamTavern.Routes.Shared.Wait (Wait)

-- | Reports a post's owner from its contact panel. Bad request is a reason
-- | that isn't one of the four or a detail that is too long. Not found is a
-- | post that isn't the game's, the viewer's own, or one either side has
-- | blocked the other from. Too many requests is a report past the viewer's
-- | allowance, which `ReportConversation` counts too.
type ReportPost =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "report") Report
    ==> OkJson ReportOutcome ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! TooManyRequestsJson Wait ! Internal_
