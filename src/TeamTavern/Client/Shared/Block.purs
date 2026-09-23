module TeamTavern.Client.Shared.Block (block, reportConversation, reportPost, unblock) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust)
import Data.Variant (onMatch)
import TeamTavern.Client.Shared.Fetch (fetchPathBody, fetchPathNoContent)
import TeamTavern.Routes.Block.Block (Block)
import TeamTavern.Routes.Block.ReportConversation (ReportConversation)
import TeamTavern.Routes.Block.ReportPost (ReportPost)
import TeamTavern.Routes.Block.Unblock (Unblock)
import TeamTavern.Routes.Shared.Report (Report)
import Type.Proxy (Proxy(..))

-- Each answers whether it worked.

block :: ∀ left. String -> Async left Boolean
block nickname = fetchPathNoContent (Proxy :: _ Block) { nickname } <#> isJust

unblock :: ∀ left. String -> Async left Boolean
unblock nickname = fetchPathNoContent (Proxy :: _ Unblock) { nickname } <#> isJust

reportPost :: ∀ left. { handle :: String, id :: Int } -> Report -> Async left Boolean
reportPost path report =
    Async.attempt (fetchPathBody (Proxy :: _ ReportPost) path report) <#> \result ->
        isJust $ hush result >>= onMatch { noContent: const $ Just unit } (const Nothing)

reportConversation :: ∀ left. Int -> Report -> Async left Boolean
reportConversation id report =
    Async.attempt (fetchPathBody (Proxy :: _ ReportConversation) { id } report) <#> \result ->
        isJust $ hush result >>= onMatch { noContent: const $ Just unit } (const Nothing)
