module TeamTavern.Routes.Block.ReportPost where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NoContent, NotAuthorized_, NotFound_, PostJson_)
import TeamTavern.Routes.Shared.Report (Report)

-- | Reports a post's owner from its contact panel. Bad request is a reason
-- | that isn't one of the four or a detail that is too long. Not found is a
-- | post that isn't the game's, the viewer's own, or one either side has
-- | blocked the other from.
type ReportPost =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "report") Report
    ==> NoContent ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! Internal_
