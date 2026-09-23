module TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail(..), sendReportEmail) where

import Prelude

import Async (Async)
import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..), maybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import TeamTavern.Routes.Shared.Report (reasons)
import TeamTavern.Server.Block.Infrastructure.AddReport (Reported, ValidReport)
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, sendEmail)

-- | Where reports are mailed (brief 10).
newtype AdminEmail = AdminEmail String

reportEmail :: AdminEmail -> Reported -> ValidReport -> Email
reportEmail (AdminEmail to) reported report = let
    post = reported.owner <> "'s " <> reported.game <> " " <> reported.type
        <> maybe " post" (\name -> " " <> name) reported.name
    reason = reasons # find (_.value >>> eq report.reason) # maybe report.reason _.label
    in
    { to
    , subject: reported.reporter <> " reported " <> reported.reported <> " on TeamTavern"
    , blocks: catMaybes
        [ Just $ Paragraph $ reported.reporter <> " reported " <> reported.reported <> " about " <> post <> "."
        , Just $ Paragraph $ "Reason: " <> reason
        , report.detail <#> \detail -> Quote $ split (Pattern "\n") detail
        , if report.block then Just $ Paragraph $ reported.reporter <> " also blocked them." else Nothing
        , Just $ Button
            { label: "Open the post"
            , path: "/games/" <> reported.handle <> "/posts/" <> show reported.post_id
            }
        ]
    , unsubscribe: false
    }

-- | A failed send is logged and doesn't fail the report, which is stored
-- | either way.
sendReportEmail :: ∀ left. Mailer -> AdminEmail -> Reported -> ValidReport -> Async left Unit
sendReportEmail mailer adminEmail reported report =
    sendEmail mailer "Error sending report email" $ reportEmail adminEmail reported report
