module TeamTavern.Client.Pages.Post.Register (Publishing, publishing, publishingPost, registerBack) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (onMatch)
import TeamTavern.Client.Shared.Fetch (fetchPath)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import Type.Proxy (Proxy(..))

-- The register step of posting (brief 6, step 4) is the sign-up and sign-in
-- pages, returning to the post screen with `?from=register`, which publishes
-- the draft once the player is back signed in.

type Publishing = { handle :: String, type_ :: String }

-- | Where the register step returns to from the post screen at the path.
registerBack :: String -> String
registerBack path = path <> "?from=register"

-- | The post an account page's `back` goes on to publish, if it does.
publishing :: String -> Maybe Publishing
publishing back = case split (Pattern "/") back of
    [ "", "games", handle, "post", typeAndQuery ] -> case split (Pattern "?") typeAndQuery of
        [ type_, "from=register" ] -> Just { handle, type_ }
        _ -> Nothing
    _ -> Nothing

-- | The post as the register step names it: "League of Legends player post".
publishingPost :: ∀ left. Publishing -> Async left (Maybe String)
publishingPost { handle, type_ } =
    Async.attempt (fetchPath (Proxy :: _ ViewGame) { handle }) <#> \result ->
        hush result >>= onMatch { ok: \game -> Just $ game.title <> " " <> type_ <> " post" } (const Nothing)
