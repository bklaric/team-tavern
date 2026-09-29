module TeamTavern.Client.Shared.Share (sharePage) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (isLeft, isRight)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Toast (Toast)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Script.Share (canShare, shareAsync)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- | Shares a page of the site by its path: through the share sheet on a touch
-- | screen, and elsewhere, or where the sheet fails, by copying its address,
-- | which the page's toast confirms.
sharePage :: ∀ left.
    (Toast (Async left) -> HookM (Async left) Unit) -> { title :: String, path :: String } -> HookM (Async left) Unit
sharePage showToast { title, path } = void $ Hooks.fork do
    url <- liftEffect $ window >>= location >>= origin <#> (_ <> path)
    sheet <- liftEffect canShare
    shared <- if sheet then H.lift $ Async.attempt (shareAsync { title, url }) <#> isRight else pure false
    unless shared do
        copied <- H.lift $ Async.attempt $ writeTextAsync url
        showToast
            { text: if isLeft copied then "The link couldn't be copied." else "Link copied."
            , action: Nothing
            }
