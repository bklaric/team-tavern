module TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail(..), sendReportEmail) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Class.Console (logShow)
import TeamTavern.Routes.Shared.Report (reasons)
import TeamTavern.Server.Block.Infrastructure.AddReport (Reported, ValidReport)
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Sendgrid (Message, sendAsync)

-- | Where reports are mailed (brief 10).
newtype AdminEmail = AdminEmail String

escape :: String -> String
escape = replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")

message :: Deployment -> AdminEmail -> Reported -> ValidReport -> Message
message deployment (AdminEmail to) reported report = let
    origin = case deployment of
        -- Only logged, and the development and test stacks serve on different ports.
        Local -> ""
        Cloud -> "https://www.teamtavern.net"
    url = origin <> "/games/" <> reported.handle <> "/posts/" <> show reported.post_id
    post = reported.owner <> "'s " <> reported.game <> " " <> reported.type
        <> maybe " post" (\name -> " " <> name) reported.name
    reason = reasons # find (_.value >>> eq report.reason) # maybe report.reason _.label
    blocked = reported.reporter <> " also blocked them."
    in
    { to
    , from: "admin@teamtavern.net"
    , subject: reported.reporter <> " reported " <> reported.reported <> " on TeamTavern"
    , html: escape reported.reporter <> " reported " <> escape reported.reported
        <> " about " <> escape post <> ".<br /><br />"
        <> "Reason: " <> escape reason <> "<br /><br />"
        <> maybe "" (\detail -> "<blockquote>" <> escape detail <> "</blockquote>") report.detail
        <> (if report.block then escape blocked <> "<br /><br />" else "")
        <> "The post: <a href=\"" <> url <> "\">" <> url <> "</a>"
    , text: reported.reporter <> " reported " <> reported.reported <> " about " <> post <> ".\n\n"
        <> "Reason: " <> reason <> "\n\n"
        <> maybe "" (\detail -> detail <> "\n\n") report.detail
        <> (if report.block then blocked <> "\n\n" else "")
        <> "The post: " <> url <> "\n"
    }

-- | A failed send is logged and doesn't fail the report, which is stored
-- | either way.
sendReportEmail :: ∀ left. Deployment -> AdminEmail -> Reported -> ValidReport -> Async left Unit
sendReportEmail deployment adminEmail reported report = do
    result <- attempt case deployment of
        Local -> logShow $ message deployment adminEmail reported report
        Cloud -> sendAsync $ message deployment adminEmail reported report
    case result of
        Left error -> fromEffect $ logError "Error sending report email" error
        Right _ -> pure unit
