module TeamTavern.Client.Shared.Block (block, reportConversation, reportPost, unblock) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Data.Variant (Variant, onMatch)
import TeamTavern.Client.Shared.AccountErrors (tryAgainIn)
import TeamTavern.Client.Shared.Fetch (expecting, fetchPathBody, fetchPathNoContent)
import TeamTavern.Routes.Block.Block (Block)
import TeamTavern.Routes.Block.ReportConversation (ReportConversation)
import TeamTavern.Routes.Block.ReportPost (ReportPost)
import TeamTavern.Routes.Block.Unblock (Unblock)
import TeamTavern.Routes.Shared.Report (Report, ReportOutcome)
import TeamTavern.Routes.Shared.Wait (Wait)
import Type.Proxy (Proxy(..))

-- A block answers whether it worked, a report how it went or why it didn't.

block :: ∀ left. String -> Async left Boolean
block nickname = fetchPathNoContent (Proxy :: _ Block) { nickname } <#> isJust

unblock :: ∀ left. String -> Async left Boolean
unblock nickname = fetchPathNoContent (Proxy :: _ Unblock) { nickname } <#> isJust

reportPost :: ∀ left. { handle :: String, id :: Int } -> Report -> Async left (Either String ReportOutcome)
reportPost path report =
    Async.attempt (fetchPathBody (expecting [ "tooManyRequests" ] (Proxy :: _ ReportPost)) path report)
    <#> reportAnswer

reportConversation :: ∀ left. Int -> Report -> Async left (Either String ReportOutcome)
reportConversation id report =
    Async.attempt (fetchPathBody (expecting [ "tooManyRequests" ] (Proxy :: _ ReportConversation)) { id } report)
    <#> reportAnswer

reportAnswer :: ∀ error others.
    Either error (Variant (ok :: ReportOutcome, tooManyRequests :: Wait | others)) -> Either String ReportOutcome
reportAnswer = case _ of
    Right response -> response # onMatch
        { ok: Right
        , tooManyRequests: \{ seconds } -> Left $ "You've sent too many reports today. " <> tryAgainIn seconds
        }
        (const $ Left couldntReport)
    Left _ -> Left couldntReport

couldntReport :: String
couldntReport = "The report couldn't be sent. Try again."
