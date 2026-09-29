module TeamTavern.Server.Block.Infrastructure.AddReport (Reported, ValidReport, validateReport, addReport) where

import Prelude

import Async (Async, left)
import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String (trim)
import Data.String.CodeUnits as CodeUnits
import Jarilo (BadRequestRow_, badRequest__)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Report (Report, reasons)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (maxLength)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Type.Row (type (+))

-- | The player a report is against and the post it is about, with what the
-- | admin's email names them by.
type Reported =
    { reporter :: String
    , reported_id :: Int
    , reported :: String
    , post_id :: Int
    , handle :: String
    , game :: String
    , type :: String
    , name :: Maybe String
    , owner :: String
    }

type ValidReport = { reason :: String, detail :: Maybe String, block :: Boolean }

-- | The detail may be as long as a message.
validateReport :: ∀ errors. Report -> Async (TerrorVar (BadRequestRow_ + errors)) ValidReport
validateReport { reason, detail, block } = let
    detail' = trim detail
    in
    if not elem reason (_.value <$> reasons)
    then left $ Terror badRequest__ [ "Report reason is unknown: " <> reason ]
    else if CodeUnits.length detail' > maxLength
    then left $ Terror badRequest__ [ "Report detail is longer than " <> show maxLength <> " characters." ]
    else pure { reason, detail: if detail' == "" then Nothing else Just detail', block }

insertQuery :: Query
insertQuery = Query """
    insert into report (reporter_id, reported_id, post_id, reason, detail)
    values ($1, $2, $3, $4, $5)
    """

blockQuery :: Query
blockQuery = Query """
    insert into block (blocker_id, blocked_id)
    values ($1, $2)
    on conflict do nothing
    """

-- | Stores the report from `reporter`, and their block where they asked for
-- | one, in the caller's transaction.
addReport :: ∀ errors. Client -> Int -> Reported -> ValidReport -> Async (InternalTerror_ errors) Unit
addReport client reporter reported report = do
    queryNone client insertQuery
        (reporter : reported.reported_id : reported.post_id : report.reason :| toNullable report.detail)
    when report.block $ queryNone client blockQuery (reporter :| reported.reported_id)
